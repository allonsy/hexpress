{-|
Module : Network.Hexpress.Types
Description : The main server types and basic functions

This module contains all the Server types along with basic functions to interact with requests and responses.
It also contains functions to convert a 'Server ()' into a Warp Application. See "Network.Hexpress.Server" and "Network.Hexpress.Request" for more high level
helper functions.
-}
module Network.Hexpress.Types (
-- * Types
  Server
, StatelessServer
-- * Helper Functions
, addHeader
, sendByteString
, setStatus
, getRequest
, performIO
, getUserState
, setUserState
-- * Converters
, passthrough
, serverToApp
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
import Data.Binary.Builder as Builder

data ServerState s = ServerState {
  req :: WAI.Request,
  toSend :: Builder,
  responseStatus :: Status,
  headers :: [Header],
  innerState :: s
}

type ServerIO s = StateT (ServerState s) IO

-- | Server type which is a monad transformer of internal monads.
-- | The 's' type is a user defined state type (can be '()' if no state is needed)
type Server s = MaybeT (ServerIO s)

-- | Server with no user defined state
type StatelessServer = Server ()

-- | Take a server computation which doesn't need an argument or its return value and turn it
-- into a server computation that passes its given argument through (namely, it returns its argument when it is done with its computation).
-- This way, if you want to insert a side-effect based server computation in between two other server computations, the inserts Server component can be inserted without
-- interrupting the data flow.
passthrough :: Server s a -> (b -> Server s b)
passthrough srv = \val -> srv >> return val

-- | Adds a header to the list of headers to send to the client.
addHeader :: (HeaderName, SB.ByteString) -> Server s ()
addHeader hd = do
  st <- ST.get
  let newST = st {headers=(headers st) ++ [hd]}
  ST.put newST

-- | Sends a ByteString to the client
sendByteString :: LB.ByteString -> Server s ()
sendByteString str = do
  st <- ST.get
  let newStr = Builder.fromLazyByteString str
  let newSt = st {toSend=Builder.append (toSend st) newStr}
  ST.put newSt

-- | Sets the Response status. Overwrites the previous status set if any.
setStatus :: Status -> Server s ()
setStatus stat = do
  st <- ST.get
  let newST = st {responseStatus=stat}
  ST.put newST

-- | returns the 'WAI.Request' object. You can use all the 'WAI.Request' functions on this object as normal.
getRequest :: Server s WAI.Request
getRequest = do
  st <- ST.get
  return $ req st

-- | Allows you to run any arbitrary IO operations without the Server monad.
performIO :: IO a -> Server s a
performIO ioact = liftIO ioact

-- | Retrieves the user state from the server
getUserState :: Server s s
getUserState = do
  st <- ST.get
  return $ innerState st

setUserState :: s -> Server s ()
setUserState newState = do
  st <- ST.get
  ST.put $ st { innerState=newState }


-- | Converts a 'Server' computation into a 'WAI.Application' which you can use in standard 'Network.Wai.Warp' apps.
-- This will also allow you to use Middleware built for warp applications with the 'Server' monad.
serverToApp :: Server s () -> s -> IO WAI.Application
serverToApp serv initialUserState = return $ \request resp -> do
  let st = runMaybeT serv -- ServerIO type
  endState <- execStateT st (ServerState request Builder.empty status200 [] initialUserState)
  let responseString = Builder.toLazyByteString $ toSend endState
  resp $ WAI.responseLBS (responseStatus endState) (headers endState) responseString
