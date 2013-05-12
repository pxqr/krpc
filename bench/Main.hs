{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Criterion.Main
import Remote.KRPC


addr :: RemoteAddr
addr = (0, 6000)

echo :: Method [Int] [Int]
echo = method "echo" ["x"] ["x"]

main :: IO ()
main = defaultMain $ map mkbench [1, 10, 100, 1000]
  where
    mkbench n = bench (show n) $ nfIO $ call addr echo [1..n]