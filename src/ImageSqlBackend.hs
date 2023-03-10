{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}

module ImageSqlBackend
    ( getByImageId,
      getAllImagesWithTags,
      getImagesWithObj,
      insertImageWithTags,
      Image,
      mkImageId,
      getImageId,
      ImageID(..),
      Tag,
      getId,
      runSqlMigration,
      tagName,
      imageContent,
      imageLabel,
      imageUrl
    ) where

import DbConnection
import GHC.Int
import Control.Monad.IO.Class  (MonadIO)
import Database.Persist hiding ((==.))
import Data.ByteString as B (ByteString)
import Database.Persist.Postgresql (ConnectionString, createPostgresqlPool)
import Database.Persist.TH (mkMigrate, mkPersist, persistLowerCase, share, sqlSettings)
import Data.Pool (Pool)
import Control.Monad.Logger (runStdoutLoggingT)
import qualified Data.ByteString.Char8 as BS
import Database.Esqueleto as E
import Control.Monad
import Data.List.Extra (groupOn)
import Data.Maybe (catMaybes)


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Image
  label String
  url String Maybe
  content B.ByteString
  deriving Show
Tag
  name String
  UniqueTagName name
  deriving Show
ImageTag
  image ImageId
  tag TagId
  deriving Show
|]

newtype ImageID = ImageID Int64

getImageId :: ImageID -> Int64
getImageId (ImageID num) = num

mkImageId :: Int64 -> ImageID
mkImageId num = ImageID num

pgConnStr :: IO (Maybe ConnectionString)
pgConnStr = do
            mConnInfo <- getDbConnectionInfoFromEnv
            case mConnInfo of
                Nothing -> return Nothing
                Just (host, port, name, user, password) ->
                   return $ Just $ BS.pack $ "host=" ++ host ++ " port=" ++ show port ++ " dbname=" ++ name ++ " user=" ++ user ++ " password=" ++ password


pgPool :: ConnectionString -> IO (Pool SqlBackend)
pgPool connStr = runStdoutLoggingT $ createPostgresqlPool connStr 10


runSqlMigration :: IO ()
runSqlMigration =
    maybe printError startPool =<< pgConnStr
      where
        printError = putStrLn "No ConnectionString found!"
        startPool connStr = do
            pool <- pgPool connStr
            putStrLn "Started migration.."
            runSqlPool (runMigration migrateAll) pool
            putStrLn "Finished migration.."


runDB :: SqlPersistT IO a -> IO a
runDB action = do
    maybeConStr <- pgConnStr
    case maybeConStr of
        Nothing -> error "No connection string found"
        Just connStr -> do
            pool <- pgPool connStr
            runSqlPool action pool

---------------------------------------------------


getByImageId_ :: MonadIO m => ImageID -> SqlPersistT m [(Entity Image, Maybe(Entity Tag))]
getByImageId_ imageId = do
      let idNum = getImageId imageId
      select $
          from $ \(image `LeftOuterJoin` imageTag `LeftOuterJoin` tag) -> do
            on (just (image ^. ImageId) ==. imageTag ?. ImageTagImage)
            on (imageTag ?. ImageTagTag ==. tag ?. TagId)
            where_ (imageTag ?. ImageTagImage ==. val (Just (toSqlKey idNum)))
            return (image, tag)


getByImageId :: ImageID -> IO [(Entity Image, [Entity Tag])]
getByImageId imageId = toGroups $ runDB $ getByImageId_ imageId

---------------------------------------------------


getAllImagesWithTags_ :: MonadIO m => SqlPersistT m [(Entity Image, Maybe (Entity Tag))]
getAllImagesWithTags_ = do
  select $
      from $ \(image `LeftOuterJoin` imageTag `LeftOuterJoin` tag) -> do
        on (imageTag ?. ImageTagTag ==.  tag ?. TagId)
        on (just (image ^. ImageId) ==. imageTag ?. ImageTagImage)
        orderBy [asc (image ^. ImageId)]
        return (image, tag)

getAllImagesWithTags :: IO [(Entity Image, [Entity Tag])]
getAllImagesWithTags = toGroups $ runDB $ getAllImagesWithTags_

--------------------------------------------------------------

-- given tag names (aka objects), get the images that have any of the tags
getImagesWithObj :: [String] -> IO [(Entity Image, [Entity Tag])]
getImagesWithObj []  = getAllImagesWithTags
getImagesWithObj tagNames = do
        tagIds <- tagNamesToTagIds tagNames
        listOfTuples <- runDB $ findImagesByTags tagIds
        toGroups $ runDB $ findImagesByID (map (entityKey . fst) listOfTuples)


findImagesByID :: [Key Image] -> SqlPersistT IO [(Entity Image, Maybe (Entity Tag))]
findImagesByID listOfIds = do
        select $ from $ \(image `LeftOuterJoin` imageTag `LeftOuterJoin` tag) -> do
            on (imageTag ?. ImageTagTag ==. tag ?. TagId)
            on (just (image ^. ImageId) ==. imageTag ?. ImageTagImage)
            where_ (image ^. ImageId `in_` valList listOfIds)
            orderBy [asc (image ^. ImageId)]
            return (image, tag)

tagNamesToTagIds :: [String] -> IO [TagId]
tagNamesToTagIds tagNames = do
        let entities = runDB $ selectList [TagName <-. tagNames] []
        (map entityKey) <$> entities


findImagesByTags :: [TagId] -> SqlPersistT IO [(Entity Image, Entity Tag)]
findImagesByTags tags = do
    select $ from $ \(image `InnerJoin` imageTag `InnerJoin` tag) -> do
        on (imageTag ^. ImageTagTag ==. tag ^. TagId)
        on (image ^. ImageId ==. imageTag ^. ImageTagImage)
        where_ (imageTag ^. ImageTagTag `in_` valList tags)
        return (image, tag)


---------------------------------------------------------------

getId :: Entity Image -> ImageID
getId img = ImageID $ fromSqlKey $ entityKey img

---------------------------------------------------------------


insertTagsIfNotExist :: [String] -> IO [TagId]
insertTagsIfNotExist tagNames = do
  tagEntities <- runDB $ selectList [TagName <-. tagNames] []
  let existingTags = map (tagName . entityVal) tagEntities
  let newTags = filter (`notElem` existingTags) tagNames
  newTagIds <- runDB $ insertMany $ map Tag newTags
  return $ newTagIds ++ (map entityKey tagEntities)


insertIntoImageTags :: ImageId -> [TagId] -> IO ()
insertIntoImageTags imageId tags = do
      let imageTagEntities = map (\tagId -> ImageTag imageId tagId) tags
      runDB $ insertMany_ imageTagEntities



insertImageWithTags :: String -> Maybe String -> B.ByteString -> [String] -> IO (Entity Image, [Entity Tag])
insertImageWithTags label url content tagNames = do
  tags <- insertTagsIfNotExist tagNames
  imgKey <- runDB $ insert $ Image label url content
  let imageId = fromSqlKey imgKey
  insertIntoImageTags imgKey tags
  head <$> (getByImageId $ ImageID imageId)


groupImagesAndMaybeTags :: [(Entity Image, Maybe b)] -> [(Entity Image, [b])]
groupImagesAndMaybeTags lst =  map  accumulate $ toGroups lst
                    where  toGroups = groupOn (entityKey . fst)

toGroups :: IO [(Entity Image, Maybe(Entity Tag))] -> IO [(Entity Image, [Entity Tag])]
toGroups lst = (liftM groupImagesAndMaybeTags) lst


accumulate :: [(a, Maybe b)] -> (a, [b])
accumulate =  (\xs -> (fst (head xs), catMaybes (map snd xs)))


mkEntity idNum value = Entity idNum value