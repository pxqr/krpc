{-# LANGUAGE IncoherentInstances #-}
module Main (main) where

import Data.BEncode
import Network.KRPC
import Network.Socket
import Shared


main :: IO ()
main = server (SockAddrInet 6000 0)
  [ unitM ==> return
  , echoM ==> return
  , echoBytes ==> return
  , swapM ==> \(a, b) -> return (b, a)
  , reverseM ==> return . reverse
  , shiftR ==> \(a, b, c) -> return (c, a, b)
  , rawM      ==> return
  , rawDictM  ==> return
  ]
