{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Remote.KRPC
import Shared


main :: IO ()
main = server 6000 [echoInt ==> return]
