-- |The 'ImageUrl' module fetches an image's content given a URL.

module ImageUrl
    ( fetchImage
    ) where

import Network.HTTP.Client (newManager, parseRequest, requestHeaders, responseStatus, responseHeaders, responseBody, method, httpLbs)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import Data.ByteString as BS
import Network.HTTP.Types.Header (hContentType)
import Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as LBS

fetchImage :: String -> IO (Either String LBS.ByteString)
fetchImage url = do
  manager <- newManager tlsManagerSettings
  request <- parseRequest url
  response <- httpLbs request manager

  let headers = responseHeaders response
      maybeContentType = lookup hContentType headers
  case maybeContentType of
    Just contentType ->
      if (BSU.fromString "image/") `BS.isPrefixOf` contentType
        then return $ Right $ responseBody response
        else return $ Left "Not an image"
    Nothing ->
      return $ Left "Missing Content-Type header"
