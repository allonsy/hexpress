{-|
Module : Network.Hexpress.Request

Module for higher level helpers dealing with the request object
-}
module Network.Hexpress.Request where

import Network.Wai
import Network.HTTP.Types
import Network.Hexpress.Types
import Data.Text
import Data.Vault.Lazy
import Data.ByteString.Lazy.Char8 as LB
import Data.Aeson as Aeson

-- | Returns the parsed query string according to the standard 'Query' type
getQueryString :: Server Query
getQueryString = do
  req <- getRequest
  return $ queryString req

-- | Get the parsed URI path for this request (the leading backslash is omitting in the path list)
getPath :: Server [Text]
getPath = do
  req <- getRequest
  return $ pathInfo req

-- | Get the Method as a bytestring
getMethod :: Server Method
getMethod = do
  req <- getRequest
  return $ requestMethod req

-- | returns the list of headers from the http request
getHeaders :: Server [Header]
getHeaders = do
  req <- getRequest
  return $ requestHeaders req


-- | The vault is an alternate way of passing information from one Server computation to another (as opposed to monadic passing from return value to arguments)
-- With the vault, you can create the vault keys in a top level function (like main) and then, use those keys to store and retrieve items of arbitrary type from the vault.
-- See the "Data.Vault.Lazy" module for more info on the API.
getVault :: Server Vault
getVault = do
  req <- getRequest
  return $ vault req

-- | gets the request body (like in a post request) in a lazy fashion.
getBodyLazy :: Server LB.ByteString
getBodyLazy = do
  req <- getRequest
  performIO $ lazyRequestBody req

-- | Gets the request body in a strict manner. A lazy bytestring is still returned but processing happens in a strict and eager way.
getBodyStrict :: Server LB.ByteString
getBodyStrict = do
  req <- getRequest
  performIO $ strictRequestBody req

-- | Parses the body of the request into a JSON Literal.
-- If parsing fails, then 'Nothing' is returned. 
getJSONObj :: Server (Maybe Aeson.Value)
getJSONObj = getJSON

-- | Parses the body of the request directly into a native data type if it implements the 'FromJSON' class.
-- if the parsing from json fails, 'Nothing' is returned.
getJSON :: FromJSON a => Server (Maybe a)
getJSON = do
  body <- getBodyLazy
  return $ Aeson.decode body
