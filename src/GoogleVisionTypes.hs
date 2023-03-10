{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Types used for communication with the Google Cloud Vision API. Specifically,
-- | the types are used for feature "OBJECT_LOCALIZATION"
module GoogleVisionTypes
    ( VisionRequests(..),
      VisionRequest(..),
      VisionImage(..),
      VisionFeature(..),
      VisionResponses(..),
      VisionResponse(..),
      EntityAnnotations(..)
    ) where

import Data.ByteString as BS
import GHC.Generics (Generic)
import Data.Aeson as J ((.=), object, FromJSON(..), ToJSON(..), toJSON)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.ByteString.Base64 as B64


data VisionRequests = VisionRequests
  { requests :: [VisionRequest]
  } deriving (Generic, Show)

data VisionRequest = VisionRequest
  { image :: VisionImage
  , features :: [VisionFeature]
  } deriving (Generic, Show)

data VisionImage = VisionImage
  { content :: BS.ByteString
  } deriving (Generic, Show)

data VisionFeature = VisionFeature
    { featureType :: String
    , maxResults :: Int
    } deriving (Generic, Show)

--------------------------------------

data VisionResponses = VisionResponses
    { responses :: [VisionResponse]
    } deriving (Generic, Show)

data VisionResponse = VisionResponse
    { localizedObjectAnnotations :: [EntityAnnotations]
    } deriving (Generic, Show)

data EntityAnnotations = EntityAnnotations
    { name :: String
    } deriving (Generic, Show)

instance FromJSON VisionResponses
instance FromJSON VisionResponse
instance FromJSON EntityAnnotations

instance J.ToJSON BS.ByteString where
  toJSON =  J.toJSON . byteStringToText

instance J.ToJSON VisionRequests where
  toJSON (VisionRequests requests) =
    J.object [ "requests" J..= requests ]

instance J.ToJSON VisionRequest where
  toJSON (VisionRequest image feature) =
    J.object [ "image" J..= image , "features" J..= feature ]

instance J.ToJSON VisionImage where
  toJSON (VisionImage content) =
    J.object ["content" J..= (content)]

instance J.ToJSON VisionFeature where
  toJSON (VisionFeature ftype mresults) =
    J.object ["type" J..= ftype, "maxResults" J..= mresults]


byteStringToText :: BS.ByteString -> T.Text
byteStringToText = E.decodeUtf8 . B64.encode
