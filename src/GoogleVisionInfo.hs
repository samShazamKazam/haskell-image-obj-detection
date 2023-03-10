
module GoogleVisionInfo
           ( getGoogleVisionInfoFromEnv
           ) where

import System.Environment

getGoogleVisionInfoFromEnv :: IO (Maybe (String))
getGoogleVisionInfoFromEnv = do
  apiKey <- lookupEnv "GOOGLE_API_KEY"

  return (apiKey)