{-|
Module : Network.Hexpress.Server

Contains basic helper functions for making servers and Middlewares.
You can use this module to send Strings, files, and JSON. You can also set custom headers.
There are functions to add debugging statements, and run servers
-}
module Network.Hexpress.Server
( -- * Headers and Sending Data
addCustomHeader
, setMimeType
, sendString
, sendJSONLiteral
, sendJSON
, staticFile
, staticFileCached
, staticDir
-- * Handler Helpers
, end
, debugLog
-- * Settings
-- | The settings object used below is the same as in 'Network.Wai.Handler.Warp.Settings'. You can use the exact same functions as in that module
-- for this settings object. Below are some common settings functions from that module have been re-exported for your convenience.
, WAI.setOnOpen
, WAI.setBeforeMainLoop
-- * Running apps
, run
, runTLS
, runEnv
, runEnvTLS
, runSettings
, runTLSSettings
) where

import Network.Hexpress.Types
import Network.Hexpress.Request
import Data.CaseInsensitive as CI
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Char8 as SB
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe
import Data.Aeson as Aeson
import Data.Text as TXT
import Network.HTTP.Types.URI
import Network.Mime
import qualified Network.Wai.Handler.Warp as WAI
import qualified Network.Wai.Handler.WarpTLS as TLS
import System.FilePath
import Data.Maybe
import Control.Exception (catch, IOException)
import System.Environment


-- | Functionally the same as 'Network.Hexpress.Types.AddHeader' except with more common types.
addCustomHeader :: (SB.ByteString, SB.ByteString) -> Server s ()
addCustomHeader (name, contents) = addHeader (CI.mk name, contents)

-- | Set the mime type to be sent. The default is "application/octet-stream"
setMimeType :: SB.ByteString -> Server s ()
setMimeType mtype = do
  addHeader (hContentType, mtype)

-- | 'sendJSONLiteral' sends a 'Aeson.Value' to the client. The mime type is automatically set.
sendJSONLiteral :: Aeson.Value -> Server s ()
sendJSONLiteral = sendJSON

-- | If you have an object that is an instance of the 'ToJSON' class, you can send it
-- directly without having to convert it first. The mime type is automatically set for you.
sendJSON :: ToJSON a => a -> Server s ()
sendJSON obj = do
  setMimeType (SB.pack "application/json")
  setStatus status200
  sendByteString (Aeson.encode obj)

-- | Send a string literal. No mime type is set
sendString :: String -> Server s ()
sendString str = do
  sendByteString $ LB.pack str

-- | This will end all communication with the client. All communication that was sent before this call will still be sent.
-- However, any 'Server' functions or Middleware following this will not be executed.
end :: Server s a
end = MaybeT (return Nothing)

-- | Sends a string to standard output for debugging.
debugLog :: String -> Server s ()
debugLog str = liftIO $ Prelude.putStrLn str

stringToPath :: String -> [TXT.Text]
stringToPath str = decodePathSegments $ SB.pack str

handleFileError :: IOException -> IO (Maybe a)
handleFileError _ = return Nothing

-- | given a filename and a mimetype for the file, a server will be generated that will serve the file contents to the client.
-- This handler will read in the file everytime its endpoint is hit by a client.
staticFile :: String -> SB.ByteString -> Server s ()
staticFile fname mime = do
  setMimeType mime
  setStatus status200
  contents <- liftIO $ catch (LB.readFile fname >>= (\cont -> return (Just cont))) handleFileError
  case contents of Nothing       -> notFound ()
                   Just fcontents -> sendByteString fcontents

-- | Same as 'staticFile' except that the file is read in during server startup and is cached. If the file changes on disk during the lifetime of the server,
-- the new version will not be sent.
--
-- It is important that this function is called in some top level IO function (like main or something similar (DO NOT CALL 'performIO' ON THIS AS IT WILL NOT CACHE PROPERLY)), then, extract the inner server and use it in your server chain.
-- The file caching happens whenever this function is called. Therefore, call this function whenever you want the file to be read in.
staticFileCached :: String -> SB.ByteString -> IO (Server s ())
staticFileCached fname mime = do
  contents <- LB.readFile fname
  let serv = addHeader (hContentType, mime) >>
             setStatus status200 >>
             sendByteString contents
  return serv

getfname :: [TXT.Text] -> [TXT.Text] -> Maybe [TXT.Text]
getfname [] path = Just path
getfname (_:_) [] = Nothing
getfname (x:xs) (y:ys)
  | xs == [] && x == TXT.empty = Just (y:ys)
  | x == y = getfname xs ys
  | otherwise = Nothing

-- | The first argument is a prefix which designates the path prefix of where this handler lies in the server tree. The second argument is the path to the folder where the static files will be served.
-- The mime type is inferred from the file extension and set automatically.
-- For example:
--
-- > staticDir "/static" "./static/files"
--
-- will only pay attention to http requests with path 'static/...' and extract the file name and find it in the './static/files' directory.
-- For example, an http request to '/static/hello/file.txt' will serve './static/files/hello/file.txt'
-- Files that aren't found will return 404.
staticDir :: String -> String -> Server s ()
staticDir prefix location = staticDirHelper where
  prefixPath = stringToPath prefix
  locationPath = stringToPath location
  staticDirHelper = do
    path <- getPath
    let localname = getfname prefixPath path
    if localname == Nothing then notFound ()
    else do
      let fname = joinPath $ Prelude.map TXT.unpack (locationPath ++ fromJust localname)
      let mtype = defaultMimeLookup (TXT.pack fname)
      staticFile fname mtype

notFound :: a -> Server s b
notFound _ = do
  setStatus status404
  end

-- | Runs with 'WAI.defaultSettings' with the port provided.
run :: Int -> Server s () -> s -> IO ()
run port srv initialUserState = do
  app <- serverToApp srv initialUserState
  WAI.run port app

-- | Runs with TLS enabled according to the provided 'TLS.TLSSettings'. The server listens on the provided port.
runTLS :: Int -> TLS.TLSSettings -> Server s () -> s -> IO ()
runTLS port settings srv initialUserState = do
  let srvSettings = WAI.setPort port WAI.defaultSettings
  app <- serverToApp srv initialUserState
  TLS.runTLS settings srvSettings app

-- | Runs on the port specified by the 'PORT' environment variable.
-- If the variable is not set or not an integer, the provided Int is used as the port.
runEnv :: Int -> Server s () -> s -> IO ()
runEnv port srv initialUserState = do
  app <- serverToApp srv initialUserState
  WAI.runEnv port app

-- | Same as 'runEnv' except with TLS settings.
runEnvTLS :: Int -> TLS.TLSSettings -> Server s () -> s -> IO ()
runEnvTLS port settings srv initialUserState = do
  pnumMaybe <- lookupEnv "PORT"
  case pnumMaybe of Nothing         -> runTLS port settings srv initialUserState
                    Just pnumString ->
                      (case reads pnumString of [(pnum, "")] -> runTLS pnum settings srv initialUserState
                                                _            -> runTLS port settings srv initialUserState)

-- | Runs the given server according to the provided 'Network.Wai.Handler.Settings'.
runSettings :: WAI.Settings -> Server s () -> s -> IO ()
runSettings settings srv initialUserState = do
  app <- serverToApp srv initialUserState
  WAI.runSettings settings app

-- | Same as 'runSettings' but with additional TLS Settings.
runTLSSettings :: TLS.TLSSettings -> WAI.Settings -> Server s () -> s -> IO ()
runTLSSettings tlsSettings srvSettings srv initialUserState = do
  app <- serverToApp srv initialUserState
  TLS.runTLS tlsSettings srvSettings app
