module Hex.Types
( Server
, Middleware
, standalone
, serverToApp
, addHeader
, sendByteString
, setStatus
, getRequest
) where

import qualified Network.Wai as WAI
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State.Lazy
import Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Char8 as SB
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Control.Monad.State.Class as ST

data ServerState = ServerState {
  req :: WAI.Request,
  toSend :: LB.ByteString,
  responseStatus :: Status,
  headers :: [Header]
}

type ServerIO = StateT ServerState IO

type Server = MaybeT ServerIO
type Middleware a b = a -> Server b

standalone :: Server a -> Middleware () a
standalone srv = \_ -> srv

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

serverToApp :: Server () -> IO WAI.Application
serverToApp serv = return $ \request resp -> do
  let st = runMaybeT serv -- ServerIO type
  endState <- execStateT st (ServerState request LB.empty status200 [])
  let responseString = toSend endState
  resp $ WAI.responseLBS status200 [] responseString
