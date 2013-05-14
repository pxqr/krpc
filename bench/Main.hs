{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Criterion.Main
import Remote.KRPC


addr :: RemoteAddr
addr = (0, 6000)

echo :: Method ByteString ByteString
echo = method "echo" ["x"] ["x"]

main :: IO ()
main = defaultMain $ map (mkbench 1)   [1, 10, 100, 1000, 32 * 1024]
                 ++  map (mkbench 10)  [1, 10, 100, 1000]
  where
    mkbench r n = bench (show r ++ "/" ++ show n) $ nfIO $
                  replicateM r $ call addr echo (B.replicate n 0)

{-
  forM_ [1..] $ const $ do
    async addr myconcat (replicate 100 [1..10])
-}
