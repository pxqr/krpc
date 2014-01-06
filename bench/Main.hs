{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Main (main) where
import Control.Monad
import Control.Monad.Logger
import Control.Monad.Reader
import Criterion.Main
import Data.ByteString as BS
import Network.KRPC

instance KRPC ByteString ByteString where
  method = "echo"

instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return ()

echo :: Handler IO
echo = handler $ \ _ bs -> return (bs :: ByteString)

addr :: SockAddr
addr = SockAddrInet 6000 (256 * 256 * 256 + 127)

main :: IO ()
main = withManager def addr [echo] $ \ m -> (`runReaderT` m) $ do
    listen
    liftIO $ defaultMain (benchmarks m)
  where
    sizes        = [10, 100, 1000, 10000, 16 * 1024]
    repetitions  = [1, 10, 100, 1000]
    benchmarks m = [mkbench m r s | r <- repetitions, s <- sizes]
      where
        mkbench action r n =
          bench (show r ++ "times" ++ "/" ++ show n ++ "bytes") $ nfIO $
            replicateM r $
              runReaderT (query addr (BS.replicate n 0)) action
