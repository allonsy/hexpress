module Main where

import Network.Hexpress.Types
import Network.Hexpress.Server
import Network.Hexpress.Middleware.Router


sendHello :: Server ()
sendHello = sendString "hello, World!"

routes :: [(Method, String, Server ())]
routes = [
  (GET, "/", sendHello),
  (GET, "/static/*", staticDir "/static/" ".")
  ]

main :: IO ()
main = do
  let app = standaloneRouter routes
  run 3000 app
