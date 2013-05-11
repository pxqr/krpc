module Shared (echoInt) where

import Remote.KRPC

echoInt :: Method Int Int
echoInt = idM
