{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import System.Environment
import Remote.KRPC
import Shared


addr :: RemoteAddr
addr = (0, 6000)

main :: IO ()
main = print =<< call addr swapM (1, 2)

{-
  forM_ [1..] $ const $ do
    async addr myconcat (replicate 100 [1..10])
-}
