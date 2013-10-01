{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.ByteString (ByteString)
import Network.KRPC
import Network.Socket


echo :: Method ByteString ByteString
echo = method "echo" ["x"] ["x"]

main :: IO ()
main = server (SockAddrInet 6000 0) [ echo ==> return ]
