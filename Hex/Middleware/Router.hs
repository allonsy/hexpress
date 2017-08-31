module Hex.Middleware.Router (
Method(..)
, router
, standaloneRouter
) where

import qualified Data.HashMap.Strict as HM
import qualified Data.ByteString.Char8 as SB
import qualified Data.Text as TXT
import Hex.Types
import Hex.Request
import Hex.Server
import Data.Hashable
import Network.HTTP.Types.URI
import Network.HTTP.Types.Status
data PathTree a b = PathTree {
  leaf :: HM.HashMap SB.ByteString (a -> Server b),
  branch :: HM.HashMap TXT.Text (PathTree a b)
}

data Method = GET
  | HEAD
  | POST
  | PUT
  | DELETE
  | CONNECT
  | OPTIONS
  | TRACE
  | PATCH
  deriving (Eq)

methodToString :: Method -> String
methodToString GET = "GET"
methodToString POST = "POST"
methodToString PUT = "PUT"
methodToString PATCH = "PATCH"
methodToString DELETE = "DELETE"
methodToString HEAD = "HEAD"
methodToString CONNECT = "CONNECT"
methodToString OPTIONS = "OPTIONS"
methodToString TRACE = "TRACE"

stringToMethod :: String -> Method
stringToMethod "GET" = GET
stringToMethod "POST" = POST
stringToMethod "PUT" = PUT
stringToMethod "PATCH" = PATCH
stringToMethod "DELETE" = DELETE
stringToMethod "HEAD" = HEAD
stringToMethod "CONNECT" = CONNECT
stringToMethod "OPTIONS" = OPTIONS
stringToMethod "TRACE" = TRACE

instance Hashable Method where
  hashWithSalt n m = hashWithSalt n (methodToString m)

byteStringToMethod :: SB.ByteString -> Method
byteStringToMethod bs = stringToMethod $ SB.unpack bs

empty :: PathTree a b
empty = PathTree HM.empty HM.empty

insert :: (Method,[TXT.Text]) -> (a -> Server b) -> PathTree a b -> PathTree a b
insert (meth,[]) f pt = pt {leaf=HM.insert (SB.pack (methodToString meth)) f (leaf pt)}
insert (meth,(p:ps)) f pt = pt {branch = HM.alter insertHelper p (branch pt)} where
  insertHelper Nothing = Just $ insert (meth,ps) f empty
  insertHelper (Just subPT) = Just $ insert (meth, ps) f subPT

lookup :: (SB.ByteString, [TXT.Text]) -> PathTree a b -> Maybe (a -> Server b)
lookup (meth, []) pt = HM.lookup meth (leaf pt)
lookup (meth, (x:xs)) pt = do
  nextLevel <- HM.lookup x (branch pt)
  Hex.Middleware.Router.lookup (meth, xs) nextLevel


populate :: [ ((Method, [TXT.Text]), (a -> Server b)) ] -> PathTree a b
populate = foldr (\(newPath, func) tr -> insert newPath func tr) empty

stringToPath :: String -> [TXT.Text]
stringToPath str = decodePathSegments $ SB.pack str

router :: [(Method, String, a -> Server b)] -> (a -> Server b)
router rts = routeHelper where
  handlerMap = populate $ map (\(meth, str, handler) -> ((meth, stringToPath str), handler)) rts
  routeHelper arg = do
    meth <- getMethod
    path <- getPath
    let handler = case Hex.Middleware.Router.lookup (meth, path) handlerMap of Just hand -> hand
                                                                               Nothing   -> notFound
    handler arg

notFound :: a -> Server b
notFound _ = do
  setStatus status404
  end

notAllowed :: a -> Server b
notAllowed _ = do
  setStatus status405
  end

standaloneRouter :: [(Method, String, Server a)] -> Server a
standaloneRouter rts = (router $ map (\(meth, rt, handler) -> (meth, rt, \() -> handler)) rts) ()
