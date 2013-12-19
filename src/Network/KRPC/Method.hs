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
import Data.Char
import Data.Monoid
import Data.List as L
import Data.String
import Data.Typeable
import Network.KRPC.Message


-- | Method datatype used to describe name, parameters and return
--   values of procedure. Client use a method to /invoke/, server
--   /implements/ the method to make the actual work.
--
--   We use the following fantom types to ensure type-safiety:
--
--     * param: Type of method parameters. Ordinary Tuple type used
--     to specify more than one parameter, so for example @Method
--     (Int, Int) result@ will take two arguments.
--
--     * result: Type of return value of the method. Similarly,
--     tuple used to specify more than one return value, so for
--     exsample @Method (Foo, Bar) (Bar, Foo)@ will take two arguments
--     and return two values.
--
newtype Method param result = Method MethodName
  deriving (Eq, Ord, IsString, BEncode)

instance (Typeable a, Typeable b) => Show (Method a b) where
  showsPrec _ = showsMethod

showsMethod :: forall a. forall b. Typeable a => Typeable b
            => Method a b -> ShowS
showsMethod (Method name) =
    shows name <>
    showString " :: " <>
    shows paramsTy <>
    showString " -> " <>
    shows valuesTy
  where
    impossible = error "KRPC.showsMethod: impossible"
    paramsTy = typeOf (impossible :: a)
    valuesTy = typeOf (impossible :: b)

-- | Example:
--   @
--   data Ping = Ping Text deriving BEncode
--   data Pong = Pong Text deriving BEncode
--
--   instance KRPC Ping Pong where
--     method = "ping"
--   @
class (BEncode req, BEncode resp) => KRPC req resp | req -> resp where
  method :: Method req resp

  default method :: Typeable req => Method req resp
  method = Method $ fromString $ L.map toLower $ show $ typeOf hole
    where
      hole = error "krpc.method: impossible" :: req
