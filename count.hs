module Main where

import Network.Hexpress.Types
import Network.Hexpress.Server
import Network.Hexpress.Middleware.Router
import Control.Concurrent.MVar


sendCount :: MVar Int -> StatelessServer ()
sendCount countVar = do
  count <- performIO $ takeMVar countVar
  let num = count+1
  performIO $ putMVar countVar num
  sendString $ "You are visitor number: " ++ (show num)

routes :: [(Method, String, MVar Int -> StatelessServer ())]
routes = [(GET, "/", sendCount)]

main :: IO ()
main = do
  countMVar <- newMVar 0
  let app = router routes $ countMVar
  run 3000 app ()
