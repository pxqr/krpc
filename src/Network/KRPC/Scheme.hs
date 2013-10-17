-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides message scheme validation for core protocol
--   messages from 'Remote.KRPC.Procotol'. This module should be used
--   with 'Remote.KRPC.Protocol', otherwise (if you are using 'Remote.KRPC')
--   this module seems to be useless.
--
{-# LANGUAGE DefaultSignatures      #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}
module Network.KRPC.Scheme
       ( KMessage(..)
       , KQueryScheme(..), methodQueryScheme
       , KResponseScheme(..), methodRespScheme
       ) where

import Control.Applicative
import Data.BEncode.BDict as BS
import Data.BEncode.Types as BS

import Network.KRPC.Protocol
import Network.KRPC


-- | Used to validate any message by its scheme
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


instance KMessage KError ErrorCode where
  scheme = errorCode
  {-# INLINE scheme #-}

data KQueryScheme = KQueryScheme {
    qscMethod :: MethodName
  , qscParams :: [ParamName]
  } deriving (Show, Read, Eq, Ord)

bdictKeys :: BDict -> [BKey]
bdictKeys (Cons k _ xs) = k : bdictKeys xs
bdictKeys  Nil          = []

instance KMessage KQuery KQueryScheme where
  scheme q = KQueryScheme
             { qscMethod = queryMethod q
             , qscParams = bdictKeys $ queryArgs q
             }
  {-# INLINE scheme #-}

methodQueryScheme :: Method a b -> KQueryScheme
methodQueryScheme = KQueryScheme <$> methodName <*> methodParams
{-# INLINE methodQueryScheme #-}

newtype KResponseScheme = KResponseScheme
  { rscVals :: [ValName]
  } deriving (Show, Read, Eq, Ord)

instance KMessage KResponse KResponseScheme where
  {-# SPECIALIZE instance KMessage KResponse KResponseScheme #-}
  scheme = KResponseScheme . bdictKeys . respVals
  {-# INLINE scheme #-}

methodRespScheme :: Method a b -> KResponseScheme
methodRespScheme = KResponseScheme . methodVals
{-# INLINE methodRespScheme #-}
