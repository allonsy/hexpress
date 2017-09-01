module Hex.Middleware.Router (
Method(..)
, router
, standaloneRouter
) where

import qualified Data.ByteString.Char8 as SB
import qualified Data.Text as TXT
import Hex.Types
import Hex.Request
import Hex.Server
import Data.Hashable
import Network.HTTP.Types.URI
import Network.HTTP.Types.Status

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

emptyText :: TXT.Text
emptyText = TXT.pack ""

starText :: TXT.Text
starText = TXT.pack "*"

isColon :: TXT.Text -> Bool
isColon str = TXT.head str == ':'

byteStringToMethod :: SB.ByteString -> Method
byteStringToMethod bs = stringToMethod $ SB.unpack bs

stringToPath :: String -> [TXT.Text]
stringToPath str = decodePathSegments $ SB.pack str

type Route a b = (Method, [TXT.Text], (a -> Server b))

extractRtPath :: Route a b -> [TXT.Text]
extractRtPath (_, p, _) = p

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

isMatch :: [TXT.Text] -> Route a b -> Bool
isMatch [] (_, [], _) = True
isMatch [] (_,(x:xs),_) = False
isMatch (y:ys) (_, [], _) = False
isMatch (y:ys) (m,(x:xs),fn)
  | x == emptyText || x == starText = True
  | isColon x = isMatch ys (m,xs,fn)
  | x == y = isMatch ys (m,xs,fn)
  | otherwise = False

isMethodMatch :: Method -> Route a b -> Bool
isMethodMatch targetMeth (meth, _, _) = meth == targetMeth

-- assumes non empty
getLast :: [a] -> a
getLast [x] = x
getLast (x:xs) = getLast xs

isExact :: Route a b -> Bool
isExact (_, [], _) = True
isExact (m, (x:xs), fn)
  | x == starText || x == emptyText || isColon x = False
  | otherwise = isExact (m, xs, fn)

findLongest :: Int -> [Route a b] -> [Route a b] -> Route a b
findLongest largest ls [] = getLast ls
findLongest largest ls (rt@(m,x,fn):xs)
  | isExact rt = rt
  | length x > largest = findLongest (length x) [rt] xs
  | length x == largest = findLongest largest (rt:ls) xs
  | otherwise = findLongest largest ls xs

router :: [(Method, String, a -> Server b)] -> (a -> Server b)
router rts arg = routerHelper where
  preprocessed = map (\(m, str, fn) -> (m, stringToPath str, fn)) rts
  routerHelper = do
    path <- getPath
    let matches = filter (isMatch path) preprocessed
    if isEmpty matches then notFound arg
    else do
      meth <- getMethod
      let targetMeth = byteStringToMethod meth
      let methMatches = filter (isMethodMatch targetMeth) matches
      if isEmpty methMatches then notAllowed arg
      else do
        let (_, _, fn) = findLongest (-1) [] methMatches
        fn arg


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
