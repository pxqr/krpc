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
import Data.List as L
import Data.Map  as M
import Data.Set  as S
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
          -> Method param result -> param -> IO ()
queryCall sock addr m arg = sendMessage q addr sock
  where
    q = kquery (L.head (methodName m)) [(L.head (methodParams m), toBEncode arg)]

-- TODO check scheme
getResult :: BEncodable result
          => KRemote -> KRemoteAddr
          -> Method param result -> IO result
getResult sock addr m = do
  resp <- recvResponse addr sock
  case resp of
    Left e -> throw (RPCException e)
    Right (respVals -> dict) -> do
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

type HandlerBody remote = (BEncode -> remote (Result BEncode), KResponseScheme)

type MethodHandler remote = (KQueryScheme, HandlerBody remote)


-- we can safely erase types in (==>)
(==>) :: forall (remote :: * -> *) (param :: *) (result :: *).
           (BEncodable param, BEncodable result)
        => Monad remote
        => Method param result
        -> (param -> remote result)
        -> MethodHandler remote
m ==> body = (methodQueryScheme m, (newbody, methodRespScheme m))
  where
    newbody x = case fromBEncode x of
                    Right a -> liftM (Right . toBEncode) (body a)
                    Left e  -> return (Left e)


-- TODO: allow forkIO
-- TODO: allow overloading
server :: (MonadBaseControl IO remote, MonadIO remote)
       => PortNumber
       -> [MethodHandler remote]
       -> remote ()
server servport handlers = do
    remoteServer servport $ \_ q -> do
      case dispatch (scheme q) of
        Nothing -> return (Left (MethodUnknown "method"))
        Just (m, rsc) -> do
          let arg = snd (L.head (M.toList (queryArgs q)))

          res <- invoke m arg
          let valName = L.head (S.toList (rscVals rsc))
          return $ bimap (ProtocolError . T.pack)
                         (kresponse . return .  (,) valName) res
  where
    handlerMap = M.fromList handlers

--    dispatch :: KQueryScheme -> MethodHandler remote
    dispatch s | Just m <-  M.lookup s handlerMap = return m
               | otherwise                        = Nothing

--    invoke :: MethodHandler remote -> BEncode -> remote BEncode
    invoke m args = m args

    bimap f _ (Left  x) = Left (f x)
    bimap _ g (Right x) = Right (g x)