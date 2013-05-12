{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Remote.KRPC


echo :: Method [Int] [Int]
echo = method "echo" ["x"] ["x"]

main :: IO ()
main = server 6000 [ echo ==> return ]
