{-# LANGUAGE OverloadedStrings #-}
module Remote.KRPC.Method
       ( Method(methodName, methodParams, methodVals, methodBody)

         -- * Construction
       , method

         -- * Predefined methods
       , idM, composeM, concatM
       ) where

import Prelude hiding ((.), id)
import Control.Category
import Control.Monad

import Remote.KRPC.Protocol

-- | The
--
--   * argument: type of method parameter
--
--   * remote: A monad used by server-side.
--
--   * result: type of return value of the method.
--
data Method remote param result = Method {
    -- | Name used in query and
    methodName   :: [MethodName]

    -- | Description of each method parameter in right to left order.
  , methodParams :: [ParamName]

    -- | Description of each method return value in right to left order.
  , methodVals   :: [ValName]

    -- | Description of method body.
  , methodBody   :: param -> remote result
  }

instance Monad remote => Category (Method remote) where
  id  = idM
  (.) = composeM

-- | Remote identity function. Could be used for echo servers for example.
--
--   idM = method "id" ["x"] ["y"] return
--
idM :: Monad m => Method m a a
idM = method "id" ["x"] ["y"] return

-- | Pipelining of two or more methods.
--
--   NOTE: composed methods will work only with this implementation of
--   KRPC, so both server and client should use this implementation,
--   otherwise you more likely get the 'ProtocolError'.
--
composeM :: Monad m => Method m b c -> Method m a b -> Method m a c
composeM g h = Method (methodName g ++ methodName h)
                      (methodParams h)
                      (methodVals g)
                      (methodBody h >=> methodBody g)

-- | Concat list of list. Could be used for performance tests.
--
--   concatM = method "concat" ["xxs"] ["xs"] $ return . Prelude.concat
--
concatM :: Monad m => Method m [[a]] [a]
concatM = method "concat" ["xxs"] ["xs"] $ return . Prelude.concat


method :: MethodName
       -> [ParamName]
       -> [ValName]
       -> (param -> remote result)
       -> Method remote param result
method name = Method [name]
