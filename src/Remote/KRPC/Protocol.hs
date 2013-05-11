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
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts, TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures #-}
module Remote.KRPC.Protocol
       (
         -- * Message
         KMessage(..)

         -- * Error
       , KError(..), errorCode, mkKError

         -- * Query
       , KQuery(queryMethod, queryParams), MethodName, ParamName, kquery

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
import Data.Set as S
import Data.Text as T
import Network.Socket hiding (recvFrom)
import Network.Socket.ByteString



-- | Used to validate message by its scheme
--
--  forall m. m `validate` scheme m
--
class KMessage message scheme | message -> scheme where
  -- | Get a message scheme.
  scheme :: message -> scheme

  -- | Check a message with a scheme.
  validate :: message -> scheme -> Bool

  default validate :: Eq scheme => message -> scheme -> Bool
  validate = (==) . scheme
  {-# INLINE validate #-}


-- TODO document that it is and how transferred
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

instance KMessage KError ErrorCode where
  {-# SPECIALIZE instance KMessage KError ErrorCode #-}
  scheme = errorCode
  {-# INLINE scheme #-}

type ErrorCode = Int

errorCode :: KError -> ErrorCode
errorCode (GenericError _)  = 201
errorCode (ServerError _)   = 202
errorCode (ProtocolError _) = 203
errorCode (MethodUnknown _) = 204
{-# INLINE errorCode #-}

mkKError :: ErrorCode -> Text -> KError
mkKError 201 = GenericError
mkKError 202 = ServerError
mkKError 203 = ProtocolError
mkKError 204 = MethodUnknown
mkKError _   = GenericError
{-# INLINE mkKError #-}



-- TODO Asc everywhere


type MethodName = ByteString
type ParamName  = ByteString

-- TODO document that it is and how transferred
data KQuery = KQuery {
    queryMethod :: MethodName
  , queryParams :: Map ParamName BEncode
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
{-# INLINE kquery #-}

data KQueryScheme = KQueryScheme {
    qscMethod :: MethodName
  , qscParams :: Set ParamName
  } deriving (Show, Read, Eq, Ord)

domen :: Map a b -> Set a
domen = error "scheme.domen"

instance KMessage KQuery KQueryScheme where
  {-# SPECIALIZE instance KMessage KQuery KQueryScheme #-}
  scheme q = KQueryScheme (queryMethod q) (domen (queryParams q))
  {-# INLINE scheme #-}

type ValName = ByteString

-- TODO document that it is and how transferred
newtype KResponse = KResponse {
    respVals :: Map ValName BEncode
  } deriving (Show, Read, Eq, Ord)

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
{-# INLINE kresponse #-}

newtype KResponseScheme = KResponseScheme {
    rscVals :: Set ValName
  } deriving (Show, Read, Eq, Ord)

instance KMessage KResponse KResponseScheme where
  {-# SPECIALIZE instance KMessage KResponse KResponseScheme #-}
  scheme = KResponseScheme . domen . respVals
  {-# INLINE scheme #-}


type KRemoteAddr = (HostAddress, PortNumber)

remoteAddr :: KRemoteAddr -> SockAddr
remoteAddr = SockAddrInet <$> snd <*> fst
{-# INLINE remoteAddr #-}


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