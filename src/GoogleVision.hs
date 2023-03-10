{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- |The 'GoogleVision' module sends an image's content to a Google Vision endpoint for object detection.
module GoogleVision
    (
     getLabelsForImageContent,
     GoogleError(..)
    ) where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.ByteString as BS
import qualified Data.ByteString.Lazy as LB (ByteString)
import qualified Data.ByteString.Char8 as Char8 (pack)
import Data.Aeson as J (encode, decode)
import Network.HTTP.Client (newManager, parseRequest, requestHeaders, requestBody, responseStatus, responseBody, method, httpLbs,  RequestBody(RequestBodyLBS))
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status
import GoogleVisionTypes
import Data.ByteString.UTF8 as BSU

googleVisionUrl :: String
googleVisionUrl = "https://vision.googleapis.com/v1/images:annotate"

googleVisionFeature :: String
googleVisionFeature = "OBJECT_LOCALIZATION"

data GoogleError = ServiceDown {code :: Int, msg :: String} | UnparsableResponse | ConfigMissing String
         deriving Show


-- | given the api key and the content, send the content and return the response object
getLabelsForImageContent_ :: String -> BS.ByteString -> IO(Either GoogleError VisionResponses)
getLabelsForImageContent_ apiKey imageBytes = do
        eitherErrStr <- detectObjectsInImage (Char8.pack apiKey) imageBytes
        case eitherErrStr of
            Left err -> return $ Left err
            Right str -> return $ parseResponse str

-- | given the api key and the content, send the content and return the list of objects detected
getLabelsForImageContent :: String -> BS.ByteString -> IO(Either GoogleError [String])
getLabelsForImageContent key imageBytes = do
        resp <- getLabelsForImageContent_ key imageBytes
        case resp of
            Left err -> return $ Left err
            Right (VisionResponses list) -> return $ Right $
                Prelude.map (name) $
                    Prelude.concat $
                        Prelude.map (localizedObjectAnnotations) list


parseResponse :: LB.ByteString -> (Either GoogleError VisionResponses)
parseResponse byteString =  case parseResponse_ byteString of
                                Nothing -> Left UnparsableResponse
                                Just resp -> Right resp


parseResponse_ :: LB.ByteString -> Maybe VisionResponses
parseResponse_ byteString =  J.decode byteString


-- | Given the api key and content, Send an image detection request to the Google Cloud Vision API
-- | and either get the response or error.
detectObjectsInImage :: BS.ByteString -> BS.ByteString -> IO (Either GoogleError LB.ByteString)
detectObjectsInImage apiKey imageBytes = do
  -- Create the request body
  let visionRequest = VisionRequest
        { image = VisionImage { content = imageBytes }
        , features = [ VisionFeature googleVisionFeature 10 ]
        }
      visionRequests = VisionRequests { requests = [visionRequest] }
      body = RequestBodyLBS $ J.encode visionRequests

  -- Create the request
  initialRequest <- parseRequest googleVisionUrl
  let headers = [ ("X-goog-api-key", apiKey), ("Content-Type", "application/json")]
      request' = initialRequest { method = "POST", requestHeaders = headers, requestBody = body }

  -- Send the request
  manager <- newManager tlsManagerSettings
  response <- httpLbs request' manager

  -- Check the response status
  if statusIsSuccessful (responseStatus response)
    then return $ Right $ responseBody response
    else return $ Left $ ServiceDown (statusCode (responseStatus response)) (BSU.toString (statusMessage (responseStatus response)))