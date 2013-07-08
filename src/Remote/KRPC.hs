-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
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
--   For more examples see @exsamples@ or @tests@ directories.
--
--   For protocol details see 'Remote.KRPC.Protocol' module.
--
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExplicitForAll     #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE ViewPatterns       #-}
module Remote.KRPC
       ( -- * Method
         Method(..)
       , method, idM

         -- * Client
       , RemoteAddr
       , RPCException(..)
       , call, Async, async, await

         -- * Server
       , MethodHandler, (==>), server

         -- * Internal
       , call_
       , withRemote
       ) where

import Control.Applicative
import Control.Exception
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Data.BEncode
import Data.ByteString.Char8 as BC
import Data.List as L
import Data.Map  as M
import Data.Typeable
import Network

import Remote.KRPC.Protocol

-- | Method datatype used to describe name, parameters and return
--   values of procedure. Client use a method to /invoke/, server
--   /implements/ the method to make the actual work.
--
--   We use the following fantom types to ensure type-safiety:
--
--     * param: Type of method parameters. Ordinary Tuple type used
--     to specify more than one parameter, so for example @Method
--     (Int, Int) result@ will take two arguments.
--
--     * result: Type of return value of the method. Similarly,
--     tuple used to specify more than one return value, so for
--     exsample @Method (Foo, Bar) (Bar, Foo)@ will take two arguments
--     and return two values.
--
data Method param result = Method {
    -- | Name used in query.
    methodName   :: MethodName

    -- | Name of each parameter in /right to left/ order.
  , methodParams :: [ParamName]

    -- | Name of each return value in /right to left/ order.
  , methodVals   :: [ValName]
  }

-- TODO ppMethod

-- | Identity procedure signature. Could be used for echo
-- servers. Implemented as:
--
--   > idM = method "id" ["x"] ["y"]
--
idM :: Method a a
idM = method "id" ["x"] ["y"]
{-# INLINE idM #-}

-- | Makes method signature. Note that order of parameters and return
--   values are not important as long as corresponding names and types
--   are match. For exsample this is the equal definitions:
--
--   > methodA : Method (Foo, Bar) (Baz, Quux)
--   > methodA = method "mymethod" ["a", "b"] ["c", "d"]
--
--   > methodA : Method (Bar, Foo) (Quux, Baz)
--   > methodB = method "mymethod" ["b", "a"] ["d", "c"]
--
method :: MethodName -> [ParamName] -> [ValName] -> Method param result
method = Method
{-# INLINE method #-}

lookupKey :: ParamName -> Map ByteString BEncode -> Result BEncode
lookupKey x = maybe (Left ("not found key " ++ BC.unpack x)) Right . M.lookup x

extractArgs :: [ParamName] -> Map ParamName BEncode -> Result BEncode
extractArgs []  d = Right $ if M.null d then  BList [] else BDict d
extractArgs [x] d = lookupKey x d
extractArgs xs  d = BList <$> mapM (`lookupKey` d) xs
{-# INLINE extractArgs #-}

injectVals :: [ParamName] -> BEncode -> [(ParamName, BEncode)]
injectVals []  (BList []) = []
injectVals []  (BDict d ) = M.toList d
injectVals []   be        = invalidParamList [] be
injectVals [p]  arg       = [(p, arg)]
injectVals ps  (BList as) = L.zip ps as
injectVals ps   be        = invalidParamList ps be
{-# INLINE injectVals #-}

invalidParamList :: [ParamName] -> BEncode -> a
invalidParamList pl be
  = error $ "KRPC invalid parameter list: " ++ show pl ++ "\n" ++
            "while procedure args are: "    ++ show be

-- | Alias to Socket, through might change in future.
type Remote = Socket

-- | Represent any error mentioned by protocol specification that
--   'call', 'await' might throw.
--   For more details see 'Remote.KRPC.Protocol'.
--
data RPCException = RPCException KError
                  deriving (Show, Eq, Typeable)

instance Exception RPCException

-- | Address of remote can be called by client.
type RemoteAddr = KRemoteAddr

queryCall :: BEncodable param
          => KRemote -> KRemoteAddr
          -> Method param result -> param -> IO ()
queryCall sock addr m arg = sendMessage q addr sock
  where
    q = kquery (methodName m) (injectVals (methodParams m) (toBEncode arg))

getResult :: BEncodable result
          => KRemote
          -> Method param result -> IO result
getResult sock m = do
  resp <- recvResponse sock
  case resp of
    Left e -> throw (RPCException e)
    Right (respVals -> dict) -> do
      case fromBEncode =<< extractArgs (methodVals m) dict of
        Right vals -> return vals
        Left  e    -> throw (RPCException (ProtocolError (BC.pack e)))


-- | Makes remote procedure call. Throws RPCException on any error
-- occurred.
call :: (MonadBaseControl IO host, MonadIO host)
     => (BEncodable param, BEncodable result)
     => RemoteAddr          -- ^ Address of callee.
     -> Method param result -- ^ Procedure to call.
     -> param               -- ^ Arguments passed by callee to procedure.
     -> host result         -- ^ Values returned by callee from the procedure.
call addr m arg = liftIO $ withRemote $ \sock -> do call_ sock addr m arg

-- | The same as 'call' but use already opened socket.
call_ :: (MonadBaseControl IO host, MonadIO host)
     => (BEncodable param, BEncodable result)
     => Remote              -- ^ Socket to use
     -> RemoteAddr          -- ^ Address of callee.
     -> Method param result -- ^ Procedure to call.
     -> param               -- ^ Arguments passed by callee to procedure.
     -> host result         -- ^ Values returned by callee from the procedure.
call_ sock addr m arg = liftIO $ do
  queryCall sock addr m arg
  getResult sock m


-- | Asynchonous result typically get from 'async' call. Used to defer
-- return values transfer.
newtype Async result = Async { waitResult :: IO result }


-- | Query procedure call but not wait for its results. This function
--   returns 'Async' value which is handle to procedure result. Actual
--   result might be obtained with 'await'. Unable to throw
--   'RPCException', this might happen in 'await' if at all.
--
--   Note that sending multiple queries at the same time to the one
--   remote is not recommended. For exsample in the following scenario:
--
--   >  aa <- async theRemote ....
--   >  ab <- async theRemote ....
--   >  a  <- await ab
--   >  b  <- await ab
--
--   it's likely that the /a/ and /b/ values will be mixed up. So in
--   order to get correct results you need to make 'await' before the
--   next 'async'.
--
async :: MonadIO host
      => (BEncodable param, BEncodable result)
      => RemoteAddr          -- ^ Address of callee.
      -> Method param result -- ^ Procedure to call.
      -> param               -- ^ Arguments passed by callee to procedure.
      -> host (Async result) -- ^ Handle to result.
async addr m arg = do
  liftIO $ withRemote $ \sock ->
     queryCall sock addr m arg
  return $ Async $ withRemote $ \sock ->
     getResult sock m

-- | Will wait until the callee finished processing of procedure call
--   and return its results. Throws 'RPCException' on any error
--   occurred.
await :: MonadIO host
      => Async result -- ^ Obtained from the corresponding 'async'.
      -> host result  -- ^ Result values of the procedure call quered
                      --   with 'async'.
await = liftIO . waitResult
{-# INLINE await #-}


type HandlerBody remote = KQuery -> remote (Either KError KResponse)

-- | Procedure signature and implementation binded up.
type MethodHandler remote = (MethodName, HandlerBody remote)

-- we can safely erase types in (==>)
-- | Assign method implementation to the method signature.
(==>) :: forall (remote :: * -> *) (param :: *) (result :: *).
           (BEncodable param,  BEncodable result)
        => Monad remote
        => Method param result      -- ^ Signature.
        -> (param -> remote result) -- ^ Implementation.
        -> MethodHandler remote     -- ^ Handler used by server.
{-# INLINE (==>) #-}
m ==> body = (methodName m, newbody)
  where
    {-# INLINE newbody #-}
    newbody q =
      case fromBEncode =<< extractArgs (methodParams m) (queryArgs q) of
        Left  e -> return (Left (ProtocolError (BC.pack e)))
        Right a -> do
          r <- body a
          return (Right (kresponse (injectVals (methodVals m) (toBEncode r))))

infix 1 ==>

-- TODO: allow forkIO

-- | Run RPC server on specified port by using list of handlers.
--   Server will dispatch procedure specified by callee, but note that
--   it will not create new thread for each connection.
--
server :: (MonadBaseControl IO remote, MonadIO remote)
       => PortNumber              -- ^ Port used to accept incoming connections.
       -> [MethodHandler remote]  -- ^ Method table.
       -> remote ()
server servport handlers = do
    remoteServer servport $ \_ q -> do
      case dispatch (queryMethod q) of
        Nothing -> return $ Left $ MethodUnknown (queryMethod q)
        Just  m -> invoke m q
  where
    handlerMap = M.fromList handlers
    dispatch s = M.lookup s handlerMap
    invoke m q = m q
