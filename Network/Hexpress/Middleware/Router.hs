{-|
Module : Network.Hexpress.Middleware.Router

Classic Middleware Router
-}

module Network.Hexpress.Middleware.Router (
Method(..)
, RouteError(..)
, router
, routerWithErrors
, standaloneRouter
, standaloneRouterWithErrors
, stringToMethod
) where

import qualified Data.ByteString.Char8 as SB
import qualified Data.Text as TXT
import Network.Hexpress.Types
import Network.Hexpress.Request
import Network.Hexpress.Server
import Network.HTTP.Types.URI
import Network.HTTP.Types.Status

-- | Type to explain router errors.
data RouteError = NotFound -- ^ Designates a 404 type error where the path in the request doesn't match any given handler
  | BadMethod -- ^ Designates a 405 type error where a handler matches the path but no handler matches the method of the request.

-- | A data type to represent the possible HTTP methods.
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

-- | converts the string of each method to the method datatype
-- the string must be in all caps e.g. @GET@ goes to 'GET'
-- It is not recommended to use this function to construct 'Method' types, just use the actual data constructors instead.
-- Use this function only if you need to convert runtime strings to the correct method.
-- If the passed in string doesn't match a known method, 'Nothing' is returned. Otherwise, if a match is found, the method is returned wrapped in a 'Just'.
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

type Route s a b = (Method, [TXT.Text], (a -> Server s b))

isEmpty :: [a] -> Bool
isEmpty [] = True
isEmpty _ = False

isMatch :: [TXT.Text] -> Route s a b -> Bool
isMatch [] (_, [], _) = True
isMatch [] (_,(_:_),_) = False
isMatch (_:_) (_, [], _) = False
isMatch (y:ys) (m,(x:xs),fn)
  | x == emptyText || x == starText = True
  | isColon x = isMatch ys (m,xs,fn)
  | x == y = isMatch ys (m,xs,fn)
  | otherwise = False

isMethodMatch :: Method -> Route s a b -> Bool
isMethodMatch targetMeth (meth, _, _) = meth == targetMeth

-- assumes non empty
getLast :: [a] -> a
getLast [] = error "Found empty list (This is a bug, you probably want to report this)"
getLast [x] = x
getLast (_:xs) = getLast xs

isExact :: Route s a b -> Bool
isExact (_, [], _) = True
isExact (m, (x:xs), fn)
  | x == starText || x == emptyText || isColon x = False
  | otherwise = isExact (m, xs, fn)

findLongest :: Int -> [Route s a b] -> [Route s a b] -> Route s a b
findLongest _ ls [] = getLast ls
findLongest largest ls (rt@(_,x,_):xs)
  | isExact rt = rt
  | length x > largest = findLongest (length x) [rt] xs
  | length x == largest = findLongest largest (rt:ls) xs
  | otherwise = findLongest largest ls xs

-- | same as 'router' except you can provide your own error handler instead of using the default one.
routerWithErrors :: [(Method, String, a -> Server s b)] -> (RouteError -> a -> Server s b) -> (a -> Server s b)
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

-- | takes in a list of triples:
--
-- > (Method, String, a -> Server b)
--
-- This triple repesents a route where if the method and path (the String) match the path and method of the incoming request, the corresponding Server handler is fired.
-- See the description of this module for more in depth examples.
-- If a match isn't found then a default error handler is fired which sends a 404 or 405 (depending on the case) and closes the connection.
router :: [(Method, String, a -> Server s b)] -> (a -> Server s b)
router rts = routerWithErrors rts defaultErrorHandler


defaultErrorHandler :: RouteError -> (a -> Server s b)
defaultErrorHandler NotFound = notFound
defaultErrorHandler BadMethod = notAllowed

notFound :: a -> Server s b
notFound _ = do
  setStatus status404
  end

notAllowed :: a -> Server s b
notAllowed _ = do
  setStatus status405
  end

-- | same as 'standaloneRouter' except with custom error handlers.
standaloneRouterWithErrors :: [(Method, String, Server s a)] -> (RouteError -> Server s a) -> Server s a
standaloneRouterWithErrors rts errorFunc = (routerWithErrors ( map (\(meth, rt, handler) -> (meth, rt, \() -> handler)) rts) (\re _ -> errorFunc re) ) ()

-- | similar to 'router' except each handler doesn't need an incoming argument.
standaloneRouter :: [(Method, String, Server s a)] -> Server s a
standaloneRouter rts = standaloneRouterWithErrors rts (\re -> defaultErrorHandler re ())
