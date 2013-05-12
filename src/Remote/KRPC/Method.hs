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

         -- * Construction
       , method

         -- * Predefined methods
       , idM
       ) where

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
