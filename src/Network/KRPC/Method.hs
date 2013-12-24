-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   Normally, you don't need to import this module.
--
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE DefaultSignatures          #-}
module Network.KRPC.Method
       ( Method (..)
       , KRPC (..)
       ) where

import Data.BEncode (BEncode)
import Data.ByteString.Char8 as BC
import Data.Char
import Data.Monoid
import Data.List as L
import Data.String
import Data.Typeable
import Network.KRPC.Message


-- | Method datatype used to describe method name, parameters and
-- return values of procedure. Client use a method to /invoke/, server
-- /implements/ the method to make the actual work.
--
--   We use the following fantom types to ensure type-safiety:
--
--     * param: Type of method parameters.
--
--     * result: Type of return value of the method.
--
newtype Method param result = Method MethodName
  deriving (Eq, Ord, IsString, BEncode)

-- | Example:
--
--   @show (Method \"concat\" :: [Int] Int) == \"concat :: [Int] -> Int\"@
--
instance (Typeable a, Typeable b) => Show (Method a b) where
  showsPrec _ = showsMethod

showsMethod :: forall a. forall b. Typeable a => Typeable b
            => Method a b -> ShowS
showsMethod (Method name) =
    showString (BC.unpack name) <>
    showString " :: " <>
    shows paramsTy <>
    showString " -> " <>
    shows valuesTy
  where
    impossible = error "KRPC.showsMethod: impossible"
    paramsTy = typeOf (impossible :: a)
    valuesTy = typeOf (impossible :: b)

-- | In order to perform or handle KRPC query you need to provide
--   corresponding 'KRPC' class.
--
--   Example:
--
--   @
--   data Ping = Ping Text deriving BEncode
--   data Pong = Pong Text deriving BEncode
--
--   instance 'KRPC' Ping Pong where
--     method = \"ping\"
--   @
--
class (BEncode req, BEncode resp) => KRPC req resp | req -> resp where
  -- | Method name. Default implementation uses lowercased @req@
  -- datatype name.
  --
  method :: Method req resp

  -- TODO add underscores
  default method :: Typeable req => Method req resp
  method = Method $ fromString $ L.map toLower $ show $ typeOf hole
    where
      hole = error "krpc.method: impossible" :: req
