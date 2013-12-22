{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.KRPC.MessageSpec (spec) where
import Control.Applicative
import Data.ByteString.Lazy as BL
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Instances ()

import Data.BEncode as BE
import Network.KRPC.Message

instance Arbitrary ErrorCode where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary KError where
  arbitrary = KError <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary KQuery where
  arbitrary = KQuery <$> pure (BInteger 0) <*> arbitrary <*> arbitrary

instance Arbitrary KResponse where
  arbitrary = KResponse <$> pure (BList []) <*> arbitrary

instance Arbitrary KMessage where
  arbitrary = frequency
    [ (1, Q <$> arbitrary)
    , (1, R <$> arbitrary)
    , (1, E <$> arbitrary)
    ]

spec :: Spec
spec = do
  describe "error message" $ do
    it "properly bencoded (iso)" $ property $ \ ke ->
      BE.decode (BL.toStrict (BE.encode ke)) `shouldBe` Right (ke :: KError)

    it "properly bencoded" $ do
      BE.decode "d1:eli201e23:A Generic Error Ocurrede1:t2:aa1:y1:ee"
        `shouldBe` Right (KError GenericError "A Generic Error Ocurred" "aa")

      BE.decode "d1:eli202e22:A Server Error Ocurrede1:t2:bb1:y1:ee"
        `shouldBe` Right (KError ServerError "A Server Error Ocurred" "bb")

      BE.decode "d1:eli203e24:A Protocol Error Ocurrede1:t2:cc1:y1:ee"
        `shouldBe` Right (KError ProtocolError "A Protocol Error Ocurred" "cc")

      BE.decode "d1:eli204e30:Attempt to call unknown methode1:t2:dd1:y1:ee"
        `shouldBe` Right
          (KError MethodUnknown "Attempt to call unknown method" "dd")

  describe "query message" $ do
    it "properly bencoded (iso)" $ property $ \ kq ->
      BE.decode (BL.toStrict (BE.encode kq)) `shouldBe` Right (kq :: KQuery)

    it "properly bencoded" $ do
      BE.decode "d1:ale1:q4:ping1:t2:aa1:y1:qe" `shouldBe`
        Right (KQuery (BList []) "ping" "aa")


  describe "response message" $ do
    it "properly bencoded (iso)" $ property $ \ kr ->
      BE.decode (BL.toStrict (BE.encode kr)) `shouldBe` Right (kr :: KResponse)

    it "properly bencoded" $ do
      BE.decode "d1:rle1:t2:aa1:y1:re" `shouldBe`
        Right (KResponse (BList []) "aa")

  describe "generic message" $ do
    it "properly bencoded (iso)" $ property $ \ km ->
      BE.decode (BL.toStrict (BE.encode km)) `shouldBe` Right (km :: KMessage)