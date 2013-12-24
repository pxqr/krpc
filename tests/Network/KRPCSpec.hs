{-# LANGUAGE OverloadedStrings #-}
module Network.KRPCSpec (spec) where
import Control.Monad.Reader
import Network.Socket (SockAddr (..))
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

spec :: Spec
spec = do
  describe "query" $ do
    it "run handlers" $ do
      let int = 0xabcd :: Int
      (withManager servAddr handlers $ runReaderT $ do
         listen
         query servAddr (Echo int))
       `shouldReturn` Echo int

    it "throw timeout exception" $ do
      (withManager servAddr handlers $ runReaderT $ do
         query servAddr (Echo (0xabcd :: Int))
       )
        `shouldThrow` (== KError GenericError "timeout expired" "0")
