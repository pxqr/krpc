{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Criterion.Main
import Network.KRPC
import Network.Socket


addr :: RemoteAddr
addr = SockAddrInet 6000 0

echo :: Method ByteString ByteString
echo = method "echo" ["x"] ["x"]

main :: IO ()
main = withRemote $ \remote -> do {
  ; let sizes = [10, 100, 1000, 10000, 16 * 1024]
  ; let repetitions = [1, 10, 100, 1000]
  ; let params = [(r, s) | r <- repetitions, s <- sizes]
  ; let benchmarks = map (uncurry (mkbench_ remote)) params
  ; defaultMain benchmarks
  }
  where
    mkbench_ re r n = bench (show r ++ "/" ++ show n) $ nfIO $
                  replicateM r $ call_ re addr echo (B.replicate n 0)

{-
  forM_ [1..] $ const $ do
    async addr myconcat (replicate 100 [1..10])
-}
