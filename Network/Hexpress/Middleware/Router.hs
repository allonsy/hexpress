module Network.Hexpress.Middleware.Router (
Method(..)
, router
, routerWithErrors
, standaloneRouter
, standaloneRouterWithErrors
) where

import qualified Data.ByteString.Char8 as SB
import qualified Data.Text as TXT
import Network.Hexpress.Types
import Network.Hexpress.Request
import Network.Hexpress.Server
import Network.HTTP.Types.URI
import Network.HTTP.Types.Status

data RouteError = NotFound | BadMethod

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

stringToMethod :: String -> Maybe Method
stringToMethod "GET" = Just GET
stringToMethod "POST" = Just POST
stringToMethod "PUT" = Just PUT
stringToMethod "PATCH" = Just PATCH
stringToMethod "DELETE" = Just DELETE
stringToMethod "HEAD" = Just HEAD
stringToMethod "CONNECT" = Just CONNECT
stringToMethod "OPTIONS" = Just OPTIONS
stringToMethod "TRACE" = Just TRACE
stringToMethod _ = Nothing

emptyText :: TXT.Text
emptyText = TXT.pack ""

starText :: TXT.Text
starText = TXT.pack "*"

isColon :: TXT.Text -> Bool
isColon str = TXT.head str == ':'

byteStringToMethod :: SB.ByteString -> Maybe Method
byteStringToMethod bs = stringToMethod $ SB.unpack bs

stringToPath :: String -> [TXT.Text]
stringToPath str = decodePathSegments $ SB.pack str

type Route a b = (Method, [TXT.Text], (a -> Server b))

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

isMatch :: [TXT.Text] -> Route a b -> Bool
isMatch [] (_, [], _) = True
isMatch [] (_,(_:_),_) = False
isMatch (_:_) (_, [], _) = False
isMatch (y:ys) (m,(x:xs),fn)
  | x == emptyText || x == starText = True
  | isColon x = isMatch ys (m,xs,fn)
  | x == y = isMatch ys (m,xs,fn)
  | otherwise = False

isMethodMatch :: Method -> Route a b -> Bool
isMethodMatch targetMeth (meth, _, _) = meth == targetMeth

-- assumes non empty
getLast :: [a] -> a
getLast [] = error "Found empty list (This is a bug, you probably want to report this)"
getLast [x] = x
getLast (_:xs) = getLast xs

isExact :: Route a b -> Bool
isExact (_, [], _) = True
isExact (m, (x:xs), fn)
  | x == starText || x == emptyText || isColon x = False
  | otherwise = isExact (m, xs, fn)

findLongest :: Int -> [Route a b] -> [Route a b] -> Route a b
findLongest _ ls [] = getLast ls
findLongest largest ls (rt@(_,x,_):xs)
  | isExact rt = rt
  | length x > largest = findLongest (length x) [rt] xs
  | length x == largest = findLongest largest (rt:ls) xs
  | otherwise = findLongest largest ls xs

routerWithErrors :: [(Method, String, a -> Server b)] -> (RouteError -> a -> Server b) -> (a -> Server b)
routerWithErrors rts errorFunc arg = routerHelper where
  preprocessed = map (\(m, str, fn) -> (m, stringToPath str, fn)) rts
  routerHelper = do
    path <- getPath
    let matches = filter (isMatch path) preprocessed
    if isEmpty matches then (errorFunc NotFound) arg
    else do
      meth <- getMethod
      case byteStringToMethod meth of
        Nothing         -> errorFunc BadMethod arg
        Just targetMeth -> do
          let methMatches = filter (isMethodMatch targetMeth) matches
          if isEmpty methMatches then (errorFunc BadMethod) arg
          else do
            let (_, _, fn) = findLongest (-1) [] methMatches
            fn arg

router :: [(Method, String, a -> Server b)] -> (a -> Server b)
router rts = routerWithErrors rts defaultErrorHandler


defaultErrorHandler :: RouteError -> (a -> Server b)
defaultErrorHandler NotFound = notFound
defaultErrorHandler BadMethod = notAllowed

notFound :: a -> Server b
notFound _ = do
  setStatus status404
  end

notAllowed :: a -> Server b
notAllowed _ = do
  setStatus status405
  end

standaloneRouterWithErrors :: [(Method, String, Server a)] -> (RouteError -> Server a) -> Server a
standaloneRouterWithErrors rts errorFunc = (routerWithErrors ( map (\(meth, rt, handler) -> (meth, rt, \() -> handler)) rts) (\re _ -> errorFunc re) ) ()

standaloneRouter :: [(Method, String, Server a)] -> Server a
standaloneRouter rts = standaloneRouterWithErrors rts (\re -> defaultErrorHandler re ())
