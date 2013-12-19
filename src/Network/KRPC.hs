-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides safe remote procedure call. One important
--   point is exceptions and errors, so be able handle them properly
--   we need to investigate a bit about how this all works.
--   Internally, in order to make method invokation KRPC makes the
--   following steps:
--
--     * Caller serialize arguments to bencoded bytestrings;
--
--     * Caller send bytestring data over UDP to the callee;
--
--     * Callee receive and decode arguments to the method and method
--     name. If it can't decode then it send 'ProtocolError' back to the
--     caller;
--
--     * Callee search for the @method name@ in the method table.
--     If it not present in the table then callee send 'MethodUnknown'
--     back to the caller;
--
--     * Callee check if argument names match. If not it send
--     'ProtocolError' back;
--
--     * Callee make the actuall call to the plain old haskell
--       function.  If the function throw exception then callee send
--       'ServerError' back.
--
--     * Callee serialize result of the function to bencoded bytestring.
--
--     * Callee encode result to bencoded bytestring and send it back
--     to the caller.
--
--     * Caller check if return values names match with the signature
--     it called in the first step.
--
--     * Caller extracts results and finally return results of the
--     procedure call as ordinary haskell values.
--
--   If every other error occurred caller get the 'GenericError'. All
--   errors returned by callee are throwed as ordinary haskell
--   exceptions at caller side. Make sure that both callee and caller
--   uses the same method signatures and everything should be ok: this
--   KRPC implementation provides some level of safety through
--   types. Also note that both caller and callee use plain UDP, so
--   KRPC is unreliable.
--
--   Consider one tiny example. From now @caller = client@ and
--   @callee = server or remote@.
--
--   Somewhere we have to define all procedure signatures. Imagine
--   that this is a library shared between client and server:
--
--   >  factorialMethod :: Method Int Int
--   >  factorialMethod = method "factorial" ["x"] ["y"]
--
--   Otherwise you can define this code in both client and server of
--   course. But in this case you might get into troubles: you can get
--   'MethodUnknown' or 'ProtocolError' if name or type of method
--   will mismatch after not synced changes in client or server code.
--
--   Now let's define our client-side:
--
--   > main = withRemote  $ \remote -> do
--   >    result <- call remote (0, 6000) factorialMethod 4
--   >    assert (result == 24) $ print "Success!"
--
--   It basically open socket with 'withRemote' and make all the other
--   steps in 'call' as describe above. And finally our server-side:
--
--   > factorialImpl :: Int -> Int
--   > factorialImpl n = product [1..n]
--   >
--   > main = runServer [factorialMethod $ return . factorialImpl]
--
--   Here we implement method signature from that shared lib and run
--   server with runServer by passing method table in.
--
--   For async API use /async/ package, old API have been removed.
--
--   For more examples see @exsamples@ or @tests@ directories.
--
--   For protocol details see 'Remote.KRPC.Protocol' module.
--
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ViewPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FunctionalDependencies #-}
module Network.KRPC
       ( -- * Methods
         Method
       , KRPC (..)

         -- * RPC
       , handler
       , query

         -- * Manager
       , MonadKRPC (..)
       , newManager
--       , closeManager

         -- * Exceptions
       , KError (..)
       ) where

import Network.KRPC.Message
import Network.KRPC.Method
import Network.KRPC.Manager