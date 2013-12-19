{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.KRPC.Manager
       ( MonadKRPC (..)
       , newManager
       , query
       , handler
       ) where

import Control.Applicative
import Control.Arrow
import Control.Concurrent
--import Control.Exception hiding (Handler)
import Control.Exception.Lifted as Lifted hiding (Handler)
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Data.BEncode as BE
import Data.ByteString.Char8 as BC
import Data.ByteString.Lazy as BL
import Data.IORef
import Data.List as L
import Data.Map as M
import Network.KRPC.Message
import Network.KRPC.Method
import Network.Socket
import Network.Socket.ByteString as BS


type KResult = Either KError KResponse

type TransactionCounter = IORef Int
type CallId             = (TransactionId, SockAddr)
type CallRes            = MVar KResult
type PendingCalls       = IORef (Map CallId CallRes)

type HandlerBody m = SockAddr -> BValue -> m (BE.Result BValue)
type Handler     m = (MethodName, HandlerBody m)

data Manager m = Manager
  { sock               :: !Socket
  , transactionCounter :: {-# UNPACK #-} !TransactionCounter
  , pendingCalls       :: {-# UNPACK #-} !PendingCalls
  , handlers           :: [Handler m]
  }

class (MonadBaseControl IO m, MonadIO m) => MonadKRPC m where
  getManager :: m (Manager a)

sockAddrFamily :: SockAddr -> Family
sockAddrFamily (SockAddrInet  _ _    ) = AF_INET
sockAddrFamily (SockAddrInet6 _ _ _ _) = AF_INET6
sockAddrFamily (SockAddrUnix  _      ) = AF_UNIX

seedTransaction :: Int
seedTransaction = 0

newManager :: SockAddr -> IO (Manager a)
newManager servAddr = do
    sock  <- bindServ
    tran  <- newIORef seedTransaction
    calls <- newIORef M.empty
    return $ Manager sock tran calls []
  where
    bindServ = do
      let family = sockAddrFamily servAddr
      sock <- socket family Datagram defaultProtocol
      when (family == AF_INET6) $ do
        setSocketOption sock IPv6Only 0
      bindSocket sock servAddr
      return sock

sendMessage :: MonadIO m => BEncode a => Socket -> SockAddr -> a -> m ()
sendMessage sock addr a =
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

unregisterQuery :: CallId -> PendingCalls -> IO ()
unregisterQuery cid ref = do
  atomicModifyIORef' ref $ \ m -> (M.delete cid m, ())

queryResponse :: BEncode a => CallRes -> IO a
queryResponse ares = do
  res  <- readMVar ares
  case res of
    Left  e -> throwIO e
    Right (KResponse {..}) ->
      case fromBEncode respVals of
        Left e  -> throwIO (KError ProtocolError (BC.pack e) respId)
        Right a -> return a

query :: forall m a b. (MonadKRPC m, KRPC a b) => SockAddr -> a -> m b
query addr params = do
  Manager {..} <- getManager
  liftIO $ do
    tid <- genTransactionId transactionCounter
    let Method name = method :: Method a b
    let q = KQuery (toBEncode params) name tid
    ares <- registerQuery (tid, addr) pendingCalls
    sendMessage sock addr q
      `onException` unregisterQuery (tid, addr) pendingCalls
    queryResponse ares

{-----------------------------------------------------------------------
--  Handlers
-----------------------------------------------------------------------}

handler :: forall m a b. (KRPC a b, MonadKRPC m)
        => (SockAddr -> a -> m b) -> Handler m
handler body = (name, wrapper)
  where
    Method name = method :: Method a b
    wrapper addr args =
      case fromBEncode args of
        Left  e -> return $ Left e
        Right a -> (Right . toBEncode) <$> body addr a

runHandler :: MonadKRPC m => HandlerBody m -> SockAddr -> KQuery -> m KResult
runHandler handler addr KQuery {..} = wrapper `Lifted.catch` failback
  where
    wrapper = ((`decodeError` queryId) +++ (`KResponse` queryId))
           <$> handler addr queryArgs
    failback e = return $ Left $ serverError e queryId

dispatchHandler :: MonadKRPC m => KQuery -> SockAddr -> m KResult
dispatchHandler q @ KQuery {..} addr = do
  Manager {..} <- getManager
  case L.lookup queryMethod handlers of
    Nothing      -> return $ Left $ unknownMethod queryMethod queryId
    Just handler -> runHandler handler addr q

{-----------------------------------------------------------------------
--  Listener
-----------------------------------------------------------------------}

handleQuery :: MonadKRPC m => KQuery -> SockAddr -> m ()
handleQuery q addr = do
  Manager {..} <- getManager
  res <- dispatchHandler q addr
  sendMessage sock addr $ either toBEncode toBEncode res

handleResponse :: MonadKRPC m => KResult -> SockAddr -> m ()
handleResponse result addr = do
  Manager {..} <- getManager
  mcall <- undefined (addr, respId) pendingCalls
  case mcall of
    Nothing   -> return ()
    Just ares -> liftIO $ putMVar ares result

handleMessage :: MonadKRPC m => KMessage -> SockAddr -> m ()
handleMessage (Q q) = handleQuery    q
handleMessage (R r) = handleResponse (Right r)
handleMessage (E e) = handleResponse (Left  e)

maxMsgSize :: Int
maxMsgSize = 64 * 1024

listener :: MonadKRPC m => m ()
listener = do
  Manager {..} <- getManager
  forever $ do
    (bs, addr) <- liftIO $ BS.recvFrom sock maxMsgSize
    case BE.decode bs of
      Left  e -> liftIO $ sendMessage sock addr  $ unknownMessage e
      Right m -> handleMessage m addr
