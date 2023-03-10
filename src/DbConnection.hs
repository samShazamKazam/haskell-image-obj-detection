module DbConnection
           ( getDbConnectionInfoFromEnv
           ) where

import System.Environment

getDbConnectionInfoFromEnv :: IO (Maybe (String, Int, String, String, String))
getDbConnectionInfoFromEnv = do
  host <- lookupEnv "DB_HOST"
  port <- fmap read <$> lookupEnv "DB_PORT"
  name <- lookupEnv "DB_NAME"
  user <- lookupEnv "DB_USER"
  password <- lookupEnv "DB_PASSWORD"
  return $ (,,,,) <$> host <*> port <*> name <*> user <*> password

