{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Network.KRPCSpec (spec) where
import Control.Monad.Logger
import Control.Monad.Reader
import Network.KRPC
import Network.KRPC.MethodSpec hiding (spec)
import Test.Hspec

servAddr :: SockAddr
servAddr = SockAddrInet 6000 (256 * 256 * 256 + 127)

handlers :: [Handler IO]
handlers =
 [ handler $ \ _ Ping     -> return Ping
 , handler $ \ _ (Echo a) -> return (Echo (a :: Bool))
 , handler $ \ _ (Echo a) -> return (Echo (a :: Int))
 ]

instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

opts :: Options
opts = def { optQueryTimeout = 1 }

spec :: Spec
spec = do
  describe "query" $ do
    it "run handlers" $ do
      let int = 0xabcd :: Int
      (withManager opts servAddr handlers $ runReaderT $ do
         listen
         query servAddr (Echo int))
       `shouldReturn` Echo int

    it "count transactions properly" $ do
      (withManager opts servAddr handlers $ runReaderT $ do
         listen
         _ <- query servAddr (Echo (0xabcd :: Int))
         _ <- query servAddr (Echo (0xabcd :: Int))
         getQueryCount
       )
        `shouldReturn` 2

    it "throw timeout exception" $ do
      (withManager opts servAddr handlers $ runReaderT $ do
         query servAddr (Echo (0xabcd :: Int))
       )
        `shouldThrow` (== TimeoutExpired)
