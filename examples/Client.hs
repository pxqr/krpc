{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment
import Remote.KRPC
import Shared


addr :: RemoteAddr
addr = (0, 6000)

main :: IO ()
main = do
  print =<< call addr unitM ()
  print =<< call addr echoM 0
  call addr reverseM [1..1000]
  print =<< call addr swapM (0, 1)
  print =<< call addr shiftR ((), 1, [2..10])

{-
  forM_ [1..] $ const $ do
    async addr myconcat (replicate 100 [1..10])
-}
