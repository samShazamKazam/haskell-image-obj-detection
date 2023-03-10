module Main (main) where

import qualified ImageApi as Api
import qualified ImageSqlBackend
import qualified ConfigReader as CR

main :: IO ()
main = do
    maybeCfg <- CR.readConfig "config.yaml"
    case maybeCfg of
        Nothing -> putStrLn "config file config.yaml is missing"
        Just cfg -> do
                    ImageSqlBackend.runSqlMigration
                    putStrLn "Starting API server.."
                    Api.startApp (Api.AppCtx cfg)

