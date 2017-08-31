module Main where

import Network.Wai
import Network.HTTP.Types (status200)
import Network.Wai.Handler.Warp (run)
import Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Char8 as SB
import Network.HTTP.Types.Header
import Hex.Types
import Hex.Server
import Hex.Request
import Hex.Middleware.Router

import Control.Concurrent
import Control.Monad.IO.Class

application req respond = respond $
  responseLBS status200 [(hContentType, SB.pack "text/plain")] (LB.pack "Hello World")

sendHello :: Server ()
sendHello = do
  sendString "hello, world!"

sendMessage :: Server ()
sendMessage = do
  sendString "New page!"



routes :: Server () -> [(Method, String, Server ())]
routes cachedFile = [
  (GET, "/", sendHello),
  (GET, "/newpage/holding/set", sendMessage),
  (GET, "/source", cachedFile)
  ]

main :: IO ()
main = do
  servFile <- staticFileCached "hello.hs" (SB.pack "text/plain")
  app <- serverToApp $ standaloneRouter (routes servFile)
  run 3000 app
