-- |
--   Copyright   :  (c) Sam T. 2013
--   License     :  MIT
--   Maintainer  :  pxqr.sta@gmail.com
--   Stability   :  experimental
--   Portability :  portable
--
--   This module provides remote procedure call.
--
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts, DeriveDataTypeable #-}
{-# LANGUAGE ExplicitForAll, KindSignatures #-}
{-# LANGUAGE ViewPatterns #-}
module Remote.KRPC
       ( module Remote.KRPC.Method, RemoteAddr

         -- * Client
       , call, async, await

         -- * Server
       , (==>), server
       ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Data.BEncode
import Data.ByteString.Char8 as BC
import Data.List as L
import Data.Map  as M
import Data.Set  as S
import Data.Typeable
import Network

import Remote.KRPC.Protocol
import Remote.KRPC.Method


data RPCException = RPCException KError
                  deriving (Show, Eq, Typeable)

instance Exception RPCException


type RemoteAddr = KRemoteAddr


queryCall :: BEncodable param
          => Extractable param
          => KRemote -> KRemoteAddr
          -> Method param result -> param -> IO ()
queryCall sock addr m arg = sendMessage q addr sock
  where
    q = kquery (methodName m) (mkVals (methodParams m) (injector arg))
    mkVals = L.zip


extractArgs :: [ParamName] -> Map ParamName BEncode -> Result [BEncode]
extractArgs as d = mapM f as
        where
          f x | Just y <- M.lookup x d = return y
              | otherwise            = Left ("not found key " ++ BC.unpack x)
{-# INLINE extractArgs #-}

-- TODO check scheme
getResult :: BEncodable result
          => Extractable result
          => KRemote -> KRemoteAddr
          -> Method param result -> IO result
getResult sock addr m = do
  resp <- recvResponse addr sock
  case resp of
    Left e -> throw (RPCException e)
    Right (respVals -> dict) -> do
      case extractArgs (methodVals m) dict >>= extractor of
        Right vals -> return vals
        Left  e    -> throw (RPCException (ProtocolError (BC.pack e)))

-- TODO async call
-- | Makes remote procedure call. Throws RPCException if server
-- returns error or decode error occurred.
--
call :: (MonadBaseControl IO host, MonadIO host)
     => (BEncodable param, BEncodable result)
     => (Extractable param, Extractable result)
     => RemoteAddr
     -> Method param result
     -> param
     -> host result
call addr m arg = liftIO $ withRemote $ \sock -> do
  queryCall sock addr m arg
  getResult sock addr m


newtype Async result = Async { waitResult :: IO result }

-- TODO document errorneous usage
async :: MonadIO host
      => (BEncodable param, BEncodable result)
      => (Extractable param, Extractable result)
      => RemoteAddr
      -> Method param result
      -> param
      -> host (Async result)
async addr m arg = do
  liftIO $ withRemote $ \sock ->
     queryCall sock addr m arg
  return $ Async $ withRemote $ \sock ->
     getResult sock addr m

await :: MonadIO host => Async result -> host result
await = liftIO . waitResult
{-# INLINE await #-}


type HandlerBody remote = KQuery -> remote (Either KError KResponse)

type MethodHandler remote = (MethodName, HandlerBody remote)


-- we can safely erase types in (==>)
(==>) :: forall (remote :: * -> *) (param :: *) (result :: *).
           (BEncodable param,  BEncodable result)
        => (Extractable param, Extractable result)
        => Monad remote
        => Method param result
        -> (param -> remote result)
        -> MethodHandler remote
{-# INLINE (==>) #-}
m ==> body = (methodName m, newbody)
  where
    {-# INLINE newbody #-}
    newbody q =
      case extractArgs (methodParams m) (queryArgs q) >>= extractor of
        Left  e -> return (Left (ProtocolError (BC.pack e)))
        Right a -> do
          r <- body a
          return (Right (kresponse (mkVals (methodVals m) (injector r))))

    mkVals :: [ValName] -> [BEncode] -> [(ValName, BEncode)]
    mkVals = L.zip

-- TODO: allow forkIO
server :: (MonadBaseControl IO remote, MonadIO remote)
       => PortNumber
       -> [MethodHandler remote]
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

    bimap f _ (Left  x) = Left (f x)
    bimap _ g (Right x) = Right (g x)