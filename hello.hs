module Main where

import Network.Hexpress.Types
import Network.Hexpress.Server
import Network.Hexpress.Middleware.Router


sendHello :: Server ()
sendHello = sendString "hello, World!"

main :: IO ()
main = do
  let app = sendHello
  run 3000 app
