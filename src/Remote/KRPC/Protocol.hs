-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides straightforward implementation of KRPC
--   protocol. In many situations Network.KRPC should be prefered
--   since it gives more safe, convenient and high level api.
--
--   > See http://www.bittorrent.org/beps/bep_0005.html#krpc-protocol
--
{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Remote.KRPC.Protocol
       (
         -- * Error
         KError(..), errorCode, mkKError

         -- * Query
       , KQuery(..), MethodName, ParamName, kquery

         -- * Response
       , KResponse(..), ValName, kresponse
       , sendMessage, recvResponse

         -- * Remote
       , KRemote, KRemoteAddr, withRemote, remoteServer

         -- * Re-exports
       , encode, encoded, decode, decoded, toBEncode, fromBEncode
       ) where

import Control.Applicative
import Control.Exception.Lifted
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Control
import Data.BEncode
import Data.ByteString as B
import qualified Data.ByteString.Lazy as LB
import Data.Map as M
import Data.Text as T
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString


data KError
  = GenericError { errorMessage :: Text }
  | ServerError  { errorMessage :: Text }
    -- | Malformed packet, invalid arguments or bad token.
  | ProtocolError { errorMessage :: Text }
  | MethodUnknown { errorMessage :: Text }
   deriving (Show, Read, Eq, Ord)

instance BEncodable KError where
  toBEncode e = fromAssocs
    [ "y" --> ("e" :: ByteString)
    , "e" --> (errorCode e, errorMessage e)
    ]

  fromBEncode (BDict d)
    |  M.lookup "y" d == Just (BString "e") =
       uncurry mkKError <$> d >-- "e"

  fromBEncode _ = decodingError "KError"

type ErrorCode = Int

errorCode :: KError -> ErrorCode
errorCode (GenericError _)  = 201
errorCode (ServerError _)   = 202
errorCode (ProtocolError _) = 203
errorCode (MethodUnknown _) = 204

mkKError :: ErrorCode -> Text -> KError
mkKError 201 = GenericError
mkKError 202 = ServerError
mkKError 203 = ProtocolError
mkKError 204 = MethodUnknown
mkKError _   = GenericError



type MethodName = ByteString
type ParamName  = ByteString

data KQuery = KQuery {
    queryMethod :: MethodName
  , queryArgs   :: Map ParamName BEncode
  } deriving (Show, Read, Eq, Ord)

instance BEncodable KQuery where
  toBEncode (KQuery m args) = fromAssocs
    [ "y" --> ("q" :: ByteString)
    , "q" --> m
    , "a" --> BDict args
    ]

  fromBEncode (BDict d)
    | M.lookup "y" d == Just (BString "q") =
      KQuery <$> d >-- "q"
             <*> d >-- "a"

  fromBEncode _ = decodingError "KQuery"

kquery :: MethodName -> [(ParamName, BEncode)] -> KQuery
kquery name args = KQuery name (M.fromList args)




type ValName = ByteString

newtype KResponse = KResponse (Map ValName BEncode)
                    deriving (Show, Read, Eq, Ord)

instance BEncodable KResponse where
  toBEncode (KResponse vals) = fromAssocs
    [ "y" --> ("r" :: ByteString)
    , "r" --> vals
    ]


  fromBEncode (BDict d)
    | M.lookup "y" d == Just (BString "r") =
      KResponse <$> d >-- "r"

  fromBEncode _ = decodingError "KDict"

kresponse :: [(ValName, BEncode)] -> KResponse
kresponse = KResponse . M.fromList


type KRemoteAddr = (HostAddress, PortNumber)

remoteAddr :: KRemoteAddr -> SockAddr
remoteAddr = SockAddrInet <$> snd <*> fst

type KRemote = Socket

withRemote :: (MonadBaseControl IO m, MonadIO m) => (KRemote -> m a) -> m a
withRemote = bracket (liftIO (socket AF_INET Datagram defaultProtocol))
                     (liftIO .  sClose)

maxMsgSize :: Int
maxMsgSize = 16 * 1024

-- TODO eliminate toStrict
sendMessage :: BEncodable msg => msg -> KRemoteAddr -> KRemote -> IO ()
sendMessage msg (host, port) sock =
  sendAllTo sock (LB.toStrict (encoded msg)) (SockAddrInet port host)

recvResponse :: KRemoteAddr -> KRemote -> IO (Either KError KResponse)
recvResponse addr sock = do
  connect sock (remoteAddr addr)
  (raw, _) <- recvFrom sock maxMsgSize
  return $ case decoded raw of
    Right resp -> Right resp
    Left decE -> Left $ case decoded raw of
      Right kerror -> kerror
      _ -> ProtocolError (T.pack decE)

remoteServer :: (MonadBaseControl IO remote, MonadIO remote)
             => PortNumber
             -> (KRemoteAddr -> KQuery -> remote (Either KError KResponse))
             -> remote ()
remoteServer servport action = bracket (liftIO bind) (liftIO . sClose) loop
  where
    bind = do
     sock <- socket AF_INET Datagram defaultProtocol
     bindSocket sock (SockAddrInet servport iNADDR_ANY)
     return sock

    loop sock = forever $ do
      (bs, addr) <- liftIO $ recvFrom sock maxMsgSize

      case addr of
        SockAddrInet port host ->
          case decoded bs of
            Right query -> do
              res <- action (host, port) query
              case res of
                Right resp -> liftIO $ sendMessage resp (host, port) sock
                Left err   -> liftIO $ sendMessage err  (host, port) sock

            Left decodeE -> liftIO $ sendMessage rpcE (host, port) sock
              where
                rpcE = ProtocolError $ T.concat
                  ["Unable to decode query: ", T.pack (show bs), "\n"
                  ,"Specifically: ", T.pack decodeE
                  ]
        _ -> return ()



-- TODO to bencodable
instance (BEncodable a, BEncodable b) => BEncodable (a, b) where
  {-# SPECIALIZE instance (BEncodable a, BEncodable b) => BEncodable (a, b) #-}
  toBEncode (a, b) = BList [toBEncode a, toBEncode b]
  {-# INLINE toBEncode #-}

  fromBEncode be = case fromBEncode be of
    Right [a, b] -> (,) <$> fromBEncode a <*> fromBEncode b
    Right _      -> decodingError "Unable to decode a pair."
    Left  e      -> Left e
  {-# INLINE fromBEncode #-}