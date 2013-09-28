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
import Data.Map as M
import Data.Set as S

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
  {-# SPECIALIZE instance KMessage KError ErrorCode #-}
  scheme = errorCode
  {-# INLINE scheme #-}


data KQueryScheme = KQueryScheme {
    qscMethod :: MethodName
  , qscParams :: Set ParamName
  } deriving (Show, Read, Eq, Ord)

instance KMessage KQuery KQueryScheme where
  {-# SPECIALIZE instance KMessage KQuery KQueryScheme #-}
  scheme q = KQueryScheme (queryMethod q) (M.keysSet (queryArgs q))
  {-# INLINE scheme #-}

methodQueryScheme :: Method a b -> KQueryScheme
methodQueryScheme = KQueryScheme <$> methodName
                                 <*> S.fromList . methodParams
{-# INLINE methodQueryScheme #-}


newtype KResponseScheme = KResponseScheme {
    rscVals :: Set ValName
  } deriving (Show, Read, Eq, Ord)

instance KMessage KResponse KResponseScheme where
  {-# SPECIALIZE instance KMessage KResponse KResponseScheme #-}
  scheme = KResponseScheme . keysSet . respVals
  {-# INLINE scheme #-}

methodRespScheme :: Method a b -> KResponseScheme
methodRespScheme = KResponseScheme . S.fromList . methodVals
{-# INLINE methodRespScheme #-}
