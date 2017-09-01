module Network.Hexpress.Request where

import Network.Wai
import Network.HTTP.Types
import Hex.Types
import Data.Text
import Data.Vault.Lazy
import Data.ByteString.Lazy.Char8 as LB
import Data.ByteString.Char8 as SB
import Data.Aeson as Aeson

getQueryString :: Server Query
getQueryString = do
  req <- getRequest
  return $ queryString req

getPath :: Server [Text]
getPath = do
  req <- getRequest
  return $ pathInfo req

getMethod :: Server Method
getMethod = do
  req <- getRequest
  return $ requestMethod req

getHeaders :: Server [Header]
getHeaders = do
  req <- getRequest
  return $ requestHeaders req

getVault :: Server Vault
getVault = do
  req <- getRequest
  return $ vault req

getBodyLazy :: Server LB.ByteString
getBodyLazy = do
  req <- getRequest
  performIO $ lazyRequestBody req

getBodyStrict :: Server LB.ByteString
getBodyStrict = do
  req <- getRequest
  performIO $ strictRequestBody req

getJSONObj :: Server (Maybe Aeson.Value)
getJSONObj = getJSON

getJSON :: FromJSON a => Server (Maybe a)
getJSON = do
  body <- getBodyLazy
  return $ Aeson.decode body
