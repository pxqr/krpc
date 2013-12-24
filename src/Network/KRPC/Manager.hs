{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances      #-}
module Network.KRPC.Manager
       ( MonadKRPC (..)
       , Manager
       , newManager
       , closeManager
       , withManager

       , query

       , Handler
       , handler
       , listener
       , listen
       ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
import Control.Concurrent.Lifted (fork)
import Control.Exception hiding (Handler)
import Control.Exception.Lifted as Lifted (catch)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.BEncode as BE
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy as BL
import Data.IORef
import Data.List as L
import Data.Map as M
import Data.Tuple
import Network.KRPC.Message
import Network.KRPC.Method
import Network.Socket hiding (listen)
import Network.Socket.ByteString as BS
import System.Timeout


type KResult = Either KError KResponse

type TransactionCounter = IORef Int
type CallId             = (TransactionId, SockAddr)
type CallRes            = MVar KResult
type PendingCalls       = IORef (Map CallId CallRes)

type HandlerBody h = SockAddr -> BValue -> h (BE.Result BValue)
type Handler     h = (MethodName, HandlerBody h)

data Manager h = Manager
  { sock               :: !Socket
  , queryTimeout       :: !Int -- ^ in seconds
  , listenerThread     :: !(MVar ThreadId)
  , transactionCounter :: {-# UNPACK #-} !TransactionCounter
  , pendingCalls       :: {-# UNPACK #-} !PendingCalls
  , handlers           :: [Handler h]
  }

class (MonadBaseControl IO m, MonadIO m) => MonadKRPC h m | m -> h where
  getManager :: m (Manager h)

  default getManager :: MonadReader (Manager h) m => m (Manager h)
  getManager = ask

  liftHandler :: h a -> m a

  default liftHandler :: m a -> m a
  liftHandler = id

instance (MonadBaseControl IO h, MonadIO h)
      => MonadKRPC h (ReaderT (Manager h) h) where
  liftHandler = lift

sockAddrFamily :: SockAddr -> Family
sockAddrFamily (SockAddrInet  _ _    ) = AF_INET
sockAddrFamily (SockAddrInet6 _ _ _ _) = AF_INET6
sockAddrFamily (SockAddrUnix  _      ) = AF_UNIX

seedTransaction :: Int
seedTransaction = 0

defaultQueryTimeout :: Int
defaultQueryTimeout = 120

newManager :: SockAddr -> [Handler h] -> IO (Manager h)
newManager servAddr handlers = do
    sock  <- bindServ
    tref  <- newEmptyMVar
    tran  <- newIORef seedTransaction
    calls <- newIORef M.empty
    return $ Manager sock defaultQueryTimeout tref tran calls handlers
  where
    bindServ = do
      let family = sockAddrFamily servAddr
      sock <- socket family Datagram defaultProtocol
      when (family == AF_INET6) $ do
        setSocketOption sock IPv6Only 0
      bindSocket sock servAddr
      return sock

-- | Unblock all pending calls and close socket.
closeManager :: Manager m -> IO ()
closeManager Manager {..} = do
  maybe (return ()) killThread =<< tryTakeMVar listenerThread
  -- TODO unblock calls
  close sock

-- | Normally you should use Control.Monad.Trans.allocate function.
withManager :: SockAddr -> [Handler h] -> (Manager h -> IO a) -> IO a
withManager addr hs = bracket (newManager addr hs) closeManager

sendMessage :: MonadIO m => BEncode a => Socket -> SockAddr -> a -> m ()
sendMessage sock addr a = do
  liftIO $ sendManyTo sock (BL.toChunks (BE.encode a)) addr

{-----------------------------------------------------------------------
--  Client
-----------------------------------------------------------------------}

genTransactionId :: TransactionCounter -> IO TransactionId
genTransactionId ref = do
  cur <- atomicModifyIORef' ref $ \ cur -> (succ cur, cur)
  return $ BC.pack (show cur)

registerQuery :: CallId -> PendingCalls -> IO CallRes
registerQuery cid ref = do
  ares <- newEmptyMVar
  atomicModifyIORef' ref $ \ m -> (M.insert cid ares m, ())
  return ares

-- simultaneous M.lookup and M.delete guarantees that we never get two
-- or more responses to the same query
unregisterQuery :: CallId -> PendingCalls -> IO (Maybe CallRes)
unregisterQuery cid ref = do
  atomicModifyIORef' ref $ swap .
    M.updateLookupWithKey (const (const Nothing)) cid

queryResponse :: BEncode a => CallRes -> IO a
queryResponse ares = do
  res <- readMVar ares
  KResponse {..} <- either throwIO pure res
  case fromBEncode respVals of
    Right r -> pure r
    Left  e -> throwIO $ decodeError e respId

-- |
--
--  This function will throw exception if quered node respond with
--  @error@ message or timeout expires.
--
query :: forall h m a b. (MonadKRPC h m, KRPC a b) => SockAddr -> a -> m b
query addr params = do
  Manager {..} <- getManager
  liftIO $ do
    tid <- genTransactionId transactionCounter
    let Method name = method :: Method a b
    let q = KQuery (toBEncode params) name tid

    ares <- registerQuery (tid, addr) pendingCalls
    sendMessage sock addr q
      `onException` unregisterQuery (tid, addr) pendingCalls

    mres <- timeout (queryTimeout * 10 ^ (6 :: Int)) $ do
      queryResponse ares

    case mres of
      Just res -> return res
      Nothing -> do
        _ <- unregisterQuery (tid, addr) pendingCalls
        throwIO $ timeoutExpired tid

{-----------------------------------------------------------------------
--  Handlers
-----------------------------------------------------------------------}

-- | Any thrown exception will be supressed and send over wire back to
-- the quering node.
handler :: forall h a b. (KRPC a b, Monad h)
        => (SockAddr -> a -> h b) -> Handler h
handler body = (name, wrapper)
  where
    Method name = method :: Method a b
    wrapper addr args =
      case fromBEncode args of
        Left  e -> return $ Left e
        Right a -> do
          r <- body addr a
          return $ Right $ toBEncode r

runHandler :: MonadKRPC h m => HandlerBody h -> SockAddr -> KQuery -> m KResult
runHandler h addr KQuery {..} = wrapper `Lifted.catch` failback
  where
    wrapper = ((`decodeError` queryId) +++ (`KResponse` queryId))
           <$> liftHandler (h addr queryArgs)
    failback e = return $ Left $ serverError e queryId

dispatchHandler :: MonadKRPC h m => KQuery -> SockAddr -> m KResult
dispatchHandler q @ KQuery {..} addr = do
  Manager {..} <- getManager
  case L.lookup queryMethod handlers of
    Nothing -> return $ Left $ unknownMethod queryMethod queryId
    Just h  -> runHandler h addr q

{-----------------------------------------------------------------------
--  Listener
-----------------------------------------------------------------------}

handleQuery :: MonadKRPC h m => KQuery -> SockAddr -> m ()
handleQuery q addr = void $ fork $ do
  Manager {..} <- getManager
  res <- dispatchHandler q addr
  sendMessage sock addr $ either toBEncode toBEncode res

handleResponse :: MonadKRPC h m => KResult -> SockAddr -> m ()
handleResponse result addr = do
  Manager {..} <- getManager
  liftIO $ do
    let resultId = either errorId respId  result
    mcall <- unregisterQuery (resultId, addr) pendingCalls
    case mcall of
      Nothing   -> return ()
      Just ares -> putMVar ares result

handleMessage :: MonadKRPC h m => KMessage -> SockAddr -> m ()
handleMessage (Q q) = handleQuery    q
handleMessage (R r) = handleResponse (Right r)
handleMessage (E e) = handleResponse (Left  e)

maxMsgSize :: Int
maxMsgSize = 64 * 1024

listener :: MonadKRPC h m => m ()
listener = do
  Manager {..} <- getManager
  forever $ do
    (bs, addr) <- liftIO $ BS.recvFrom sock maxMsgSize
    case BE.decode bs of
      Left  e -> liftIO $ sendMessage sock addr  $ unknownMessage e
      Right m -> handleMessage m addr

-- | Should be run before any 'query', otherwise they will never
-- succeed.
listen :: MonadKRPC h m => m ()
listen = do
  Manager {..} <- getManager
  tid <- fork $ listener
  liftIO $ putMVar listenerThread tid
