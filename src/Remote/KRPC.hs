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
module Remote.KRPC
       ( module Remote.KRPC.Method, RemoteAddr

         -- * Client
       , call, async, await

         -- * Server
       , handler, server
       ) where

import Control.Exception
import Control.Monad
import Control.Monad.Trans.Control
import Control.Monad.IO.Class
import Data.BEncode
import Data.List as L
import Data.Map  as M
import Data.Text as T
import Data.Typeable
import Network

import Remote.KRPC.Protocol
import Remote.KRPC.Method


data RPCException = RPCException KError
                  deriving (Show, Eq, Typeable)

instance Exception RPCException


type RemoteAddr = KRemoteAddr


queryCall :: BEncodable param
          => KRemote -> KRemoteAddr
          -> Method remote param result -> param -> IO ()
queryCall sock addr m arg = sendMessage q addr sock
  where
    q = kquery (L.head (methodName m)) [(L.head (methodParams m), toBEncode arg)]

getResult :: BEncodable result
          => KRemote -> KRemoteAddr
          -> Method remote param result -> IO result
getResult sock addr m = do
  resp <- recvResponse addr sock
  case resp of
    Left e -> throw (RPCException e)
    Right (KResponse dict) -> do
      let valName = L.head (methodVals m)
      case M.lookup valName dict of
        Just val | Right res <- fromBEncode val -> return res
        Nothing     -> throw (RPCException (ProtocolError msg))
          where
            msg = T.concat
              [ "Unable to find return value: ", T.pack (show valName), "\n"
              , "in response: ", T.pack (show dict)
              ]

-- TODO async call
-- | Makes remote procedure call. Throws RPCException if server
-- returns error or decode error occurred.
--
call :: (MonadBaseControl IO host, MonadIO host)
     => (BEncodable param, BEncodable result)
     => RemoteAddr
     -> Method remote param result
     -> param
     -> host result
call addr m arg = liftIO $ withRemote $ \sock -> do
  queryCall sock addr m arg
  getResult sock addr m


newtype Async result = Async { waitResult :: IO result }

async :: MonadIO host
      => (BEncodable param, BEncodable result)
      => RemoteAddr
      -> Method remote param result
      -> param
      -> host (Async result)
async addr m arg = do
  liftIO $ withRemote $ \sock ->
     queryCall sock addr m arg
  return $ Async $ withRemote $ \sock ->
     getResult sock addr m

await :: MonadIO host => Async result -> host result
await = liftIO . waitResult

-- TODO better name
type MHandler remote = Method remote BEncode (Result BEncode)

handler :: forall (remote :: * -> *) (param :: *) (result :: *).
           (BEncodable param, BEncodable result)
        => Monad remote
        => Method remote param result
        -> Method remote BEncode (Result BEncode)
handler m = m { methodBody = \x -> do
                  case fromBEncode x of
                    Right a -> liftM (Right . toBEncode) (methodBody m a)
                    Left e  -> return (Left e)
              }

-- TODO: allow forkIO
server :: (MonadBaseControl IO remote, MonadIO remote)
       => PortNumber
       -> [MHandler remote]
       -> remote ()
server servport ms = remoteServer servport $ \_ q -> do
  let name = queryMethod q
  let args = queryArgs q
  let m = L.head ms
  res <- methodBody m (snd (L.head (M.toList args)))
  case res of
   Left  r -> return (Left (ProtocolError (T.pack r)))
   Right r -> do
     let retName = L.head (methodVals m)
     return (Right (kresponse [(retName, r)]))
