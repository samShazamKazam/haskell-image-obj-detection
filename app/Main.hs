module Main (main) where

import qualified ImageApi as Api
import qualified ImageSqlBackend
import ConfigReader as CR
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool, SqlBackend)
import qualified Data.ByteString.Char8 as BS
import Data.Pool (Pool)
import Control.Monad.Logger (runStdoutLoggingT)

pgConnStr :: DbConnectionInfo -> IO (Maybe ConnectionString)
pgConnStr connInfo = do
       case connInfo of
            DbConnectionInfo host port name user password ->
                return $ Just $ BS.pack $ "host=" ++ host ++ " port=" ++ show port ++ " dbname=" ++ name ++ " user=" ++ user ++ " password=" ++ password


pgPool :: ConnectionString -> IO (Pool SqlBackend)
pgPool connStr = runStdoutLoggingT $ createPostgresqlPool connStr 10

main :: IO ()
main = do
    maybeCfg <- CR.readConfig "config.yaml"
    case maybeCfg of
        Nothing -> putStrLn "config file config.yaml is missing"
        Just cfg -> do
                let dbConnInfo = db $ cfg
                maybeConnStr <- pgConnStr dbConnInfo
                maybe printErr start maybeConnStr
                where printErr = putStrLn "Could not create connection pool!"
                      start connStr = do
                                  pool <- pgPool connStr
                                  ImageSqlBackend.runSqlMigration pool
                                  putStrLn "Starting API server.."
                                  Api.startApp (Api.AppCtx cfg pool)

