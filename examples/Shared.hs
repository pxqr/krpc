module Shared (echoInt, myconcat) where

import Remote.KRPC

echoInt :: Method IO Int Int
echoInt = idM

myconcat :: Method IO [[Int]] [Int]
myconcat = concatM