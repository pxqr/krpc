-- |
--   Copyright   :  (c) Sam Truzjan 2013
--   License     :  BSD3
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides safe remote procedure call. One important
--   point is exceptions and errors, so to be able handle them
--   properly we need to investigate a bit about how this all works.
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
--   If every other error occurred then caller get the
--   'GenericError'. All errors returned by callee are throwed as
--   ordinary haskell exceptions at caller side. Also note that both
--   caller and callee use plain UDP, so KRPC is unreliable.
--
--   For async 'query' use @async@ package.
--
--   For protocol details see "Network.KRPC.Message" module.
--
module Network.KRPC
       ( -- * Methods
         Method
       , KRPC (..)

         -- * RPC
       , Handler
       , handler

         -- ** Query
       , QueryFailure (..)
       , query

         -- * Manager
       , MonadKRPC (..)
       , Options (..)
       , def
       , Manager
       , newManager
       , closeManager
       , withManager
       , listen

         -- * Exceptions
       , KError (..)
       , ErrorCode (..)

         -- * Re-export
       , SockAddr (..)
       ) where

import Data.Default.Class
import Network.KRPC.Message
import Network.KRPC.Method
import Network.KRPC.Manager
import Network.Socket (SockAddr (..))