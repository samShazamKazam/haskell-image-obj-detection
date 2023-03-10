{-# LANGUAGE DeriveGeneric #-}

module ConfigReader
           ( ServiceConfig(..), DbConnectionInfo(..), GoogleVisionInfo(..), toConnString,
           readConfig, AppCtx(..) ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Yaml as Y
import GHC.Generics
import Data.Aeson

data ServiceConfig = ServiceConfig {
    googleVision :: GoogleVisionInfo,
    db :: DbConnectionInfo
} deriving (Show, Generic)

data DbConnectionInfo = DbConnectionInfo {
    host :: String,
    port :: Int,
    dbname :: String,
    user :: String,
    password :: String
} deriving (Show, Generic)

data GoogleVisionInfo =  GoogleVisionInfo {
    url :: String,
    feature :: String,
    apiKey :: String
} deriving (Show, Generic)

data AppCtx = AppCtx { svcCfg :: ServiceConfig }

instance FromJSON ServiceConfig
instance FromJSON DbConnectionInfo
instance FromJSON GoogleVisionInfo

toConnString :: DbConnectionInfo -> BS.ByteString
toConnString info =
    BS.pack $ "host=" ++ host(info) ++ " port=" ++ show (port(info)) ++ " dbname=" ++ dbname(info) ++ " user=" ++ user(info) ++ " password=" ++ password(info)

readConfig :: FilePath -> IO (Maybe ServiceConfig)
readConfig path = do
    content <- BS.readFile path
    return $ Y.decode content
