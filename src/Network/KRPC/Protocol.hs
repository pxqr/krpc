-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides straightforward implementation of KRPC
--   protocol. In many situations 'Network.KRPC' should be prefered
--   since it gives more safe, convenient and high level api.
--
--   See <http://www.bittorrent.org/beps/bep_0005.html#krpc-protocol>
--
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE DeriveDataTypeable     #-}
module Network.KRPC.Protocol
       ( -- * Error
         KError(..)
       , serverError

         -- * Query
       , KQuery(..)
       , MethodName

         -- * Response
       , KResponse(..)
       ) where

import Control.Exception.Lifted as Lifted
import Data.BEncode as BE
import Data.BEncode.BDict as BE
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.Typeable

-- | Errors used to signal that some error occurred while processing a
-- procedure call. Error may be send only from server to client but
-- not in the opposite direction.
--
--   Errors are encoded as bencoded dictionary:
--
--   > { "y" : "e", "e" : [<error_code>, <human_readable_error_reason>] }
--
data KError
    -- | Some error doesn't fit in any other category.
  = GenericError { errorMessage :: !ByteString }

    -- | Occur when server fail to process procedure call.
  | ServerError  { errorMessage :: !ByteString }

    -- | Malformed packet, invalid arguments or bad token.
  | ProtocolError { errorMessage :: !ByteString }

    -- | Occur when client trying to call method server don't know.
  | MethodUnknown { errorMessage :: !ByteString }
   deriving (Show, Read, Eq, Ord, Typeable)

instance BEncode KError where
  {-# SPECIALIZE instance BEncode KError #-}
  {-# INLINE toBEncode #-}
  toBEncode e = toDict $
       "e" .=! (errorCode e, errorMessage e)
    .: "y" .=! ("e" :: ByteString)
    .: endDict

  {-# INLINE fromBEncode #-}
  fromBEncode be @ (BDict d)
    | BE.lookup "y" d == Just (BString "e")
    = (`fromDict` be) $ do
      uncurry mkKError <$>! "e"

  fromBEncode _ = decodingError "KError"

instance Exception KError

type ErrorCode = Int

errorCode :: KError -> ErrorCode
errorCode (GenericError _)  = 201
errorCode (ServerError _)   = 202
errorCode (ProtocolError _) = 203
errorCode (MethodUnknown _) = 204
{-# INLINE errorCode #-}

mkKError :: ErrorCode -> ByteString -> KError
mkKError 201 = GenericError
mkKError 202 = ServerError
mkKError 203 = ProtocolError
mkKError 204 = MethodUnknown
mkKError _   = GenericError
{-# INLINE mkKError #-}

serverError :: SomeException -> KError
serverError = ServerError . BC.pack . show

type MethodName = ByteString

-- | Query used to signal that caller want to make procedure call to
-- callee and pass arguments in. Therefore query may be only sent from
-- client to server but not in the opposite direction.
--
--   Queries are encoded as bencoded dictionary:
--
--    > { "y" : "q", "q" : "<method_name>", "a" : [<arg1>, <arg2>, ...] }
--
data KQuery = KQuery
  { queryMethod :: !MethodName
  , queryArgs   :: !BValue
  } deriving (Show, Read, Eq, Ord, Typeable)

instance BEncode KQuery where
  {-# SPECIALIZE instance BEncode KQuery #-}
  {-# INLINE toBEncode #-}
  toBEncode (KQuery m args) = toDict $
       "a" .=! args
    .: "q" .=! m
    .: "y" .=! ("q" :: ByteString)
    .: endDict

  {-# INLINE fromBEncode #-}
  fromBEncode bv @ (BDict d)
    | BE.lookup "y" d == Just (BString "q") = (`fromDict` bv) $ do
      a <- field (req "a")
      q <- field (req "q")
      return $! KQuery q a

  fromBEncode _ = decodingError "KQuery"

-- | KResponse used to signal that callee successufully process a
-- procedure call and to return values from procedure. KResponse should
-- not be sent if error occurred during RPC. Thus KResponse may be only
-- sent from server to client.
--
--   Responses are encoded as bencoded dictionary:
--
--   > { "y" : "r", "r" : [<val1>, <val2>, ...] }
--
newtype KResponse = KResponse
  { respVals :: BValue
  } deriving (Show, Read, Eq, Ord, Typeable)

instance BEncode KResponse where
  {-# INLINE toBEncode #-}
  toBEncode (KResponse vals) = toDict $
       "r" .=! vals
    .: "y" .=! ("r" :: ByteString)
    .: endDict

  {-# INLINE fromBEncode #-}
  fromBEncode bv @ (BDict d)
    | BE.lookup "y" d == Just (BString "r") = (`fromDict` bv) $ do
      KResponse <$>! "r"

  fromBEncode _ = decodingError "KDict"
