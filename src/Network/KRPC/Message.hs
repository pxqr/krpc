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
module Network.KRPC.Message
       ( -- * Error
         ErrorCode (..)
       , KError(..)
       , serverError

         -- * Query
       , KQuery(..)
       , MethodName

         -- * Response
       , KResponse(..)
       ) where

import Control.Applicative
import Control.Exception.Lifted as Lifted
import Data.BEncode as BE
import Data.ByteString as B
import Data.ByteString.Char8 as BC
import Data.Typeable


-- | This transaction ID is generated by the querying node and is
-- echoed in the response, so responses may be correlated with
-- multiple queries to the same node. The transaction ID should be
-- encoded as a short string of binary numbers, typically 2 characters
-- are enough as they cover 2^16 outstanding queries.
type TransactionId = ByteString

{-----------------------------------------------------------------------
-- Error messages
-----------------------------------------------------------------------}

data ErrorCode
    -- | Some error doesn't fit in any other category.
  = GenericError

    -- | Occur when server fail to process procedure call.
  | ServerError

    -- | Malformed packet, invalid arguments or bad token.
  | ProtocolError

    -- | Occur when client trying to call method server don't know.
  | MethodUnknown
    deriving (Show, Read, Eq, Ord, Bounded, Typeable)

instance Enum ErrorCode where
  fromEnum GenericError  = 201
  fromEnum ServerError   = 202
  fromEnum ProtocolError = 203
  fromEnum MethodUnknown = 204
  {-# INLINE fromEnum #-}

  toEnum 201 = GenericError
  toEnum 202 = ServerError
  toEnum 203 = ProtocolError
  toEnum 204 = MethodUnknown
  toEnum _   = GenericError
  {-# INLINE toEnum #-}

instance BEncode ErrorCode where
  toBEncode = toBEncode . fromEnum
  {-# INLINE toBEncode #-}

  fromBEncode b = toEnum <$> fromBEncode b
  {-# INLINE fromBEncode #-}

-- | Errors used to signal that some error occurred while processing a
-- procedure call. Error may be send only from server to client but
-- not in the opposite direction.
--
--   Errors are encoded as bencoded dictionary:
--
--   > { "y" : "e", "e" : [<error_code>, <human_readable_error_reason>] }
--
data KError = KError
  { errorCode    :: !ErrorCode
  , errorMessage :: !ByteString
  , errorId      :: !TransactionId
  } deriving (Show, Read, Eq, Ord, Typeable)

instance BEncode KError where

  toBEncode KError {..} = toDict $
       "e" .=! (errorCode, errorMessage)
    .: "t" .=! errorId
    .: "y" .=! ("e" :: ByteString)
    .: endDict
  {-# INLINE toBEncode #-}

  fromBEncode = fromDict $ do
    lookAhead $ match "y" (BString "e")
    (code, msg) <- field (req "e")
    KError code msg <$>! "t"
  {-# INLINE fromBEncode #-}

instance Exception KError

serverError :: SomeException -> TransactionId -> KError
serverError e = KError ServerError (BC.pack (show e))

{-----------------------------------------------------------------------
-- Query messages
-----------------------------------------------------------------------}

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
  { queryArgs   :: !BValue
  , queryMethod :: !MethodName
  , queryId     :: !TransactionId
  } deriving (Show, Read, Eq, Ord, Typeable)

instance BEncode KQuery where
  toBEncode KQuery {..} = toDict $
       "a" .=! queryArgs
    .: "q" .=! queryMethod
    .: "t" .=! queryId
    .: "y" .=! ("q" :: ByteString)
    .: endDict
  {-# INLINE toBEncode #-}

  fromBEncode = fromDict $ do
    lookAhead $ match "y" (BString "q")
    KQuery <$>! "a" <*>! "q" <*>! "t"
  {-# INLINE fromBEncode #-}

{-----------------------------------------------------------------------
-- Response messages
-----------------------------------------------------------------------}

-- | KResponse used to signal that callee successufully process a
-- procedure call and to return values from procedure. KResponse should
-- not be sent if error occurred during RPC. Thus KResponse may be only
-- sent from server to client.
--
--   Responses are encoded as bencoded dictionary:
--
--   > { "y" : "r", "r" : [<val1>, <val2>, ...] }
--
data KResponse = KResponse
  { respVals :: BValue
  , respId   :: TransactionId
  } deriving (Show, Read, Eq, Ord, Typeable)

instance BEncode KResponse where
  toBEncode KResponse {..} = toDict $
       "r" .=! respVals
    .: "t" .=! respId
    .: "y" .=! ("r" :: ByteString)
    .: endDict
  {-# INLINE toBEncode #-}

  fromBEncode = fromDict $ do
    lookAhead $ match "y" (BString "r")
    KResponse <$>! "r" <*>! "t"
  {-# INLINE fromBEncode #-}