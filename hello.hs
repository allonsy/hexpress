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

server :: Server ()
server = do
  sendString "hello, world!"
  debugLog "served hello to client"
  sendString "wait!"
  debugLog "served second to client"

main :: IO ()
main = do
  app <- serverToApp server
  run 3000 app
