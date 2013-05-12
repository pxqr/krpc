{-# LANGUAGE IncoherentInstances #-}
module Main (main) where

import Remote.KRPC
import Shared


main :: IO ()
main = server 6000 [swapM ==> \(a, b) -> return (b, a)]
