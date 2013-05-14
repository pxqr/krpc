{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString (ByteString)
import Remote.KRPC


echo :: Method ByteString ByteString
echo = method "echo" ["x"] ["x"]

main :: IO ()
main = server 6000 [ echo ==> return ]
