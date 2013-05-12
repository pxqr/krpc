{-# LANGUAGE OverloadedStrings #-}
module Shared (echoInt, swapM) where

import Remote.KRPC

echoInt :: Method Int Int
echoInt = idM

swapM :: Method (Int, Int) (Int, Int)
swapM = method "swap" ["x", "y"] ["b", "a"]

{-
type NodeId = Int
type InfoHashe = Int
type NodeAddr = Int
type Token = Int
type

ping :: Method NodeId NodeId
ping = method "ping" ["id"] ["id"]

find_node :: Method (NodeId, NodeId) (NodeId, NodeAddr)
find_node = method "find_node" ["id", "target"] ["id", "nodes"]

get_peers :: Method (NodeId :*: InfoHash) (NodeId, Token, NodeAddr :|: NodeAddr)
get_peers = method "get_peers"
  ("id", "target")
  ("id", "token", view ("values" :|: "nodes"))
view :: BEncodable -> Maybe BEncodable
view  = undefined
announce_peer :: Method (NodeId, InfoHash, PortNumber, Token) NodeId
announce_peer = undefined
-}