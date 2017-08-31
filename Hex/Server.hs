module Hex.Server
( addCustomHeader
, setMimeType
, sendString
, end
, debugLog
, performIO
, staticFile
, staticFileCached
) where

import Hex.Types
import Data.CaseInsensitive as CI
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Char8 as SB
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson as Aeson

addCustomHeader :: (SB.ByteString, SB.ByteString) -> Server ()
addCustomHeader (name, contents) = addHeader (CI.mk name, contents)

setMimeType :: SB.ByteString -> Server ()
setMimeType mtype = do
  addHeader (hContentType, mtype)

sendJSONObj :: Aeson.Value -> Server ()
sendJSONObj = sendJSON

sendJSON :: ToJSON a => a -> Server ()
sendJSON obj = do
  setMimeType (SB.pack "application/json")
  setStatus status200
  sendByteString (Aeson.encode obj)

sendString :: String -> Server ()
sendString str = do
  sendByteString $ LB.pack str

end :: Server a
end = MaybeT (return Nothing)

debugLog :: String -> Server ()
debugLog str = liftIO $ Prelude.putStrLn str

performIO :: IO a -> Server a
performIO ioact = liftIO ioact

staticFile :: String -> SB.ByteString -> Server ()
staticFile fname mime = do
  setMimeType mime
  setStatus status200
  contents <- liftIO $ LB.readFile fname
  sendByteString contents

staticFileCached :: String -> SB.ByteString -> IO (Server ())
staticFileCached fname mime = do
  contents <- LB.readFile fname
  let serv = addHeader (hContentType, mime) >>
             setStatus status200 >>
             sendByteString contents
  return serv
