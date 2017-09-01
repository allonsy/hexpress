module Hex.Types
( Server
, Middleware
, passthrough
, serverToApp
, addHeader
, sendByteString
, setStatus
, getRequest
, performIO
) where

import qualified Network.Wai as WAI
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Char8 as SB
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Control.Monad.State.Class as ST
import Control.Monad.IO.Class

data ServerState = ServerState {
  req :: WAI.Request,
  toSend :: LB.ByteString,
  responseStatus :: Status,
  headers :: [Header]
}

type ServerIO = StateT ServerState IO

type Server = MaybeT ServerIO
type Middleware a b = a -> Server b

passthrough :: Server a -> (b -> Server b)
passthrough srv = \val -> srv >> return val

addHeader :: (HeaderName, SB.ByteString) -> Server ()
addHeader hd = do
  st <- ST.get
  let newST = st {headers=(headers st) ++ [hd]}
  ST.put newST

sendByteString :: LB.ByteString -> Server ()
sendByteString str = do
  st <- ST.get
  let newSt = st {toSend=LB.append (toSend st) str}
  ST.put newSt

setStatus :: Status -> Server ()
setStatus stat = do
  st <- ST.get
  let newST = st {responseStatus=stat}
  ST.put newST

getRequest :: Server WAI.Request
getRequest = do
  st <- ST.get
  return $ req st

performIO :: IO a -> Server a
performIO ioact = liftIO ioact

serverToApp :: Server () -> IO WAI.Application
serverToApp serv = return $ \request resp -> do
  let st = runMaybeT serv -- ServerIO type
  endState <- execStateT st (ServerState request LB.empty status200 [])
  let responseString = toSend endState
  resp $ WAI.responseLBS (responseStatus endState) [] responseString
