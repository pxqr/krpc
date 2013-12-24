{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Network.KRPC.MethodSpec where
import Control.Applicative
import Data.BEncode
import Data.ByteString as BS
import Data.Typeable
import Network.KRPC
import Test.Hspec


data Ping = Ping
  deriving (Show, Eq, Typeable)

instance BEncode Ping where
  toBEncode Ping = toBEncode ()
  fromBEncode b  = Ping <$ (fromBEncode b :: Result ())

instance KRPC Ping Ping

ping :: Monad h => Handler h
ping = handler $ \ _ Ping -> return Ping

newtype Echo a = Echo a
  deriving (Show, Eq, BEncode, Typeable)

echo :: Monad h => Handler h
echo = handler $ \ _ (Echo a) -> return (Echo (a :: ByteString))

instance (Typeable a, BEncode a) => KRPC (Echo a) (Echo a)

spec :: Spec
spec = do
  describe "ping method" $ do
    it "name is ping" $ do
      (method :: Method Ping Ping) `shouldBe` "ping"

    it "has pretty Show instance" $ do
      show (method :: Method Ping Ping) `shouldBe` "ping :: Ping -> Ping"

  describe "echo method" $ do
    it "is overloadable" $ do
      (method :: Method (Echo Int ) (Echo Int )) `shouldBe` "echo int"
      (method :: Method (Echo Bool) (Echo Bool)) `shouldBe` "echo bool"

    it "has pretty Show instance" $ do
      show (method :: Method (Echo Int) (Echo Int))
        `shouldBe` "echo int :: Echo Int -> Echo Int"