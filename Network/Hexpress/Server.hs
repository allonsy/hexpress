module Network.Hexpress.Server
( addCustomHeader
, setMimeType
, sendString
, sendJSON
, sendJSONLiteral
, end
, debugLog
, staticFile
, staticFileCached
, staticDir
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

addCustomHeader :: (SB.ByteString, SB.ByteString) -> Server ()
addCustomHeader (name, contents) = addHeader (CI.mk name, contents)

setMimeType :: SB.ByteString -> Server ()
setMimeType mtype = do
  addHeader (hContentType, mtype)

sendJSONLiteral :: Aeson.Value -> Server ()
sendJSONLiteral = sendJSON

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

stringToPath :: String -> [TXT.Text]
stringToPath str = decodePathSegments $ SB.pack str

handleFileError :: IOException -> IO (Maybe a)
handleFileError _ = return Nothing

staticFile :: String -> SB.ByteString -> Server ()
staticFile fname mime = do
  setMimeType mime
  setStatus status200
  contents <- liftIO $ catch (LB.readFile fname >>= (\cont -> return (Just cont))) handleFileError
  case contents of Nothing       -> notFound ()
                   Just contents -> sendByteString contents

staticFileCached :: String -> SB.ByteString -> IO (Server ())
staticFileCached fname mime = do
  contents <- LB.readFile fname
  let serv = addHeader (hContentType, mime) >>
             setStatus status200 >>
             sendByteString contents
  return serv

getfname :: [TXT.Text] -> [TXT.Text] -> Maybe [TXT.Text]
getfname [] path = Just path
getfname (x:xs) [] = Nothing
getfname (x:xs) (y:ys)
  | xs == [] && x == TXT.empty = Just (y:ys)
  | x == y = getfname xs ys
  | otherwise = Nothing

staticDir :: String -> String -> Server ()
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

notFound :: a -> Server b
notFound _ = do
  setStatus status404
  end

-- runs with Server's settings
run :: Int -> Server () -> IO ()
run port srv = do
  app <- serverToApp srv
  WAI.run port app

runTLS :: Int -> TLS.TLSSettings -> Server () -> IO ()
runTLS port settings srv = do
  app <- serverToApp srv
  TLS.runTLS settings WAI.defaultSettings app

runEnv :: Int -> Server () -> IO ()
runEnv port srv = do
  app <- serverToApp srv
  WAI.runEnv port app

runEnvTLS :: Int -> TLS.TLSSettings -> Server () -> IO ()
runEnvTLS port settings srv = do
  pnumMaybe <- lookupEnv "PORT"
  case pnumMaybe of Nothing         -> runTLS port settings srv
                    Just pnumString ->
                      (case reads pnumString of [(pnum, "")] -> runTLS pnum settings srv
                                                _            -> runTLS port settings srv)

runSettings :: WAI.Settings -> Server () -> IO ()
runSettings settings srv = do
  app <- serverToApp srv
  WAI.runSettings settings app

runTLSSettings :: TLS.TLSSettings -> WAI.Settings -> Server () -> IO ()
runTLSSettings tlsSettings srvSettings srv = do
  app <- serverToApp srv
  TLS.runTLS tlsSettings srvSettings app
