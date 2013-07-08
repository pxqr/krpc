{-# LANGUAGE OverloadedStrings #-}
module Shared
       ( echoM
       , echoBytes
       , unitM
       , swapM
       , reverseM
       , shiftR
       , rawM
       ) where

import Data.ByteString (ByteString)
import Data.BEncode
import Remote.KRPC

unitM :: Method () ()
unitM = method "unit" [] []

echoM :: Method Int Int
echoM = method "echo" ["x"] ["x"]

echoBytes :: Method ByteString ByteString
echoBytes = method "echoBytes" ["x"] ["x"]

reverseM :: Method [Int] [Int]
reverseM = method "reverse" ["xs"] ["ys"]

swapM :: Method (Int, Int) (Int, Int)
swapM = method "swap" ["x", "y"] ["b", "a"]

shiftR :: Method ((), Int, [Int]) ([Int], (), Int)
shiftR = method "shiftR" ["x", "y", "z"] ["a", "b", "c"]

rawM :: Method BEncode BEncode
rawM = method "rawM" [""] [""]