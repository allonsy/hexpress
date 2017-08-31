module Hex.Server
( addCustomHeader
, setMimeType
, sendString
, end
, debugLog
, performIO
) where

import Hex.Types
import Data.CaseInsensitive as CI
import Network.HTTP.Types.Status
import Network.HTTP.Types.Header
import Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Char8 as SB
import Control.Monad.IO.Class
import Control.Monad.Trans.Maybe

addCustomHeader :: (SB.ByteString, SB.ByteString) -> Server ()
addCustomHeader (name, contents) = addHeader (CI.mk name, contents)

setMimeType :: SB.ByteString -> Server ()
setMimeType mtype = do
  addHeader (hContentType, mtype)

sendString :: String -> Server ()
sendString str = do
  sendByteString $ LB.pack str

end :: Server a
end = MaybeT (return Nothing)

debugLog :: String -> Server ()
debugLog str = liftIO $ Prelude.putStrLn str

performIO :: IO a -> Server a
performIO ioact = liftIO ioact
