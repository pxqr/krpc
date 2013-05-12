-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}
module Remote.KRPC.Method
       ( Method(methodName, methodParams, methodVals)
       , methodQueryScheme, methodRespScheme

         -- * Construction
       , method

         -- * Predefined methods
       , idM

         -- * Internal
       , Extractable(..)
       ) where

import Prelude hiding ((.), id)
import Control.Applicative
import Control.Category
import Control.Monad
import Data.BEncode
import Data.ByteString as B
import Data.List as L
import Data.Set as S

import Remote.KRPC.Protocol


-- | The
--
--   * argument: type of method parameter
--
--   * remote: A monad used by server-side.
--
--   * result: type of return value of the method.
--
data Method param result = Method {
    -- | Name used in query and
    methodName   :: MethodName

    -- | Description of each parameter in /right to left/ order.
  , methodParams :: [ParamName]

    -- | Description of each return value in /right to left/ order.
  , methodVals   :: [ValName]
  }
methodQueryScheme :: Method a b -> KQueryScheme
methodQueryScheme = KQueryScheme <$> methodName
                                 <*> S.fromList . methodParams
{-# INLINE methodQueryScheme #-}


methodRespScheme :: Method a b -> KResponseScheme
methodRespScheme = KResponseScheme . S.fromList . methodVals
{-# INLINE methodRespScheme #-}

-- TODO ppMethod

-- | Remote identity function. Could be used for echo servers for example.
--
--   idM = method "id" ["x"] ["y"] return
--
idM :: Method a a
idM = method "id" ["x"] ["y"]
{-# INLINE idM #-}

method :: MethodName -> [ParamName] -> [ValName] -> Method param result
method = Method
{-# INLINE method #-}



class Extractable a where
  injector :: a -> [BEncode]
  extractor :: [BEncode] -> Result a

instance (BEncodable a, BEncodable b) => Extractable (a, b) where
  {- SPECIALIZE instance (BEncodable a, BEncodable b) => Extractable (a, b) -}
  injector (a, b) = [toBEncode a, toBEncode b]
  {-# INLINE injector #-}

  extractor [a, b] = (,) <$> fromBEncode a <*> fromBEncode b
  extractor _      = decodingError "unable to match pair"
  {-# INLINE extractor #-}
{-
instance BEncodable a => Extractable a where
  {-# SPECIALIZE instance BEncodable a => Extractable a #-}

  injector x = [toBEncode x]
  {-# INLINE injector #-}

  extractor [x] = fromBEncode x
  extractor _   = decodingError "unable to match single value"
  {-# INLINE extractor #-}
-}