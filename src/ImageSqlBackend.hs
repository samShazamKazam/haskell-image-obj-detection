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
import Control.Monad.IO.Class  (MonadIO, liftIO)
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
import ConfigReader
import Control.Monad.Trans.Reader (runReaderT, ReaderT(..), asks, ask)


-- | ReaderT (r -> m a)
-- | runReaderT :: ReaderT r m a -> r -> m a
-- | ReaderT AppCtx Handler a -> ReaderT SqlBackend IO a
--transform :: AppM a -> SqlPersistT IO a
--transform appM =


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


getByImageId_ :: MonadIO m => ImageID -> SqlPersistT m [(Entity Image, Maybe(Entity Tag))]
getByImageId_ imageId = do
      let idNum = getImageId imageId
      select $
          from $ \(image `LeftOuterJoin` imageTag `LeftOuterJoin` tag) -> do
            on (just (image ^. ImageId) ==. imageTag ?. ImageTagImage)
            on (imageTag ?. ImageTagTag ==. tag ?. TagId)
            where_ (imageTag ?. ImageTagImage ==. val (Just (toSqlKey idNum)))
            return (image, tag)


getAllImagesWithTags_ :: MonadIO m => SqlPersistT m [(Entity Image, Maybe (Entity Tag))]
getAllImagesWithTags_ = do
  select $
      from $ \(image `LeftOuterJoin` imageTag `LeftOuterJoin` tag) -> do
        on (imageTag ?. ImageTagTag ==.  tag ?. TagId)
        on (just (image ^. ImageId) ==. imageTag ?. ImageTagImage)
        orderBy [asc (image ^. ImageId)]
        return (image, tag)

findImagesByID :: [Key Image] -> SqlPersistT IO [(Entity Image, Maybe (Entity Tag))]
findImagesByID listOfIds = do
        select $ from $ \(image `LeftOuterJoin` imageTag `LeftOuterJoin` tag) -> do
            on (imageTag ?. ImageTagTag ==. tag ?. TagId)
            on (just (image ^. ImageId) ==. imageTag ?. ImageTagImage)
            where_ (image ^. ImageId `in_` valList listOfIds)
            orderBy [asc (image ^. ImageId)]
            return (image, tag)

findImagesByTags :: [TagId] -> SqlPersistT IO [(Entity Image, Entity Tag)]
findImagesByTags tags = do
    select $ from $ \(image `InnerJoin` imageTag `InnerJoin` tag) -> do
        on (imageTag ^. ImageTagTag ==. tag ^. TagId)
        on (image ^. ImageId ==. imageTag ^. ImageTagImage)
        where_ (imageTag ^. ImageTagTag `in_` valList tags)
        return (image, tag)


pgPool :: ConnectionString -> IO (Pool SqlBackend)
pgPool connStr = runStdoutLoggingT $ createPostgresqlPool connStr 10


runSqlMigration :: Pool SqlBackend -> IO ()
runSqlMigration pool = do
        putStrLn "Started migration.."
        runSqlPool (runMigration migrateAll) pool
        putStrLn "Finished migration.."


type SqlReaderT = ReaderT (Pool SqlBackend) IO


runDB :: SqlPersistT IO a -> SqlReaderT a
runDB action = do
    pool <- ask
    liftIO $ runSqlPool action pool

---------------------------------------------------


getByImageId :: ImageID -> SqlReaderT [(Entity Image, [Entity Tag])]
getByImageId imageId = toGroups $ runDB $ getByImageId_ imageId

---------------------------------------------------

getAllImagesWithTags :: SqlReaderT [(Entity Image, [Entity Tag])]
getAllImagesWithTags = toGroups $ runDB $ getAllImagesWithTags_

--------------------------------------------------------------

-- given tag names (aka objects), get the images that have any of the tags
getImagesWithObj :: [String] -> SqlReaderT [(Entity Image, [Entity Tag])]
getImagesWithObj []  = getAllImagesWithTags
getImagesWithObj tagNames = do
        tagIds <- tagNamesToTagIds tagNames
        listOfTuples <- runDB $ findImagesByTags tagIds
        toGroups $ runDB $ findImagesByID (map (entityKey . fst) listOfTuples)

tagNamesToTagIds :: [String] -> SqlReaderT [TagId]
tagNamesToTagIds tagNames = do
        let entities = runDB $ selectList [TagName <-. tagNames] []
        (map entityKey) <$> entities


---------------------------------------------------------------

---------------------------------------------------------------

insertTagsIfNotExist :: [String] -> SqlReaderT [TagId]
insertTagsIfNotExist tagNames = do
  tagEntities <- runDB $ selectList [TagName <-. tagNames] []
  let existingTags = map (tagName . entityVal) tagEntities
  let newTags = filter (`notElem` existingTags) tagNames
  newTagIds <- runDB $ insertMany $ map Tag newTags
  return $ newTagIds ++ (map entityKey tagEntities)


--insertIntoImageTags :: ImageId -> [TagId] -> ReaderT (Pool SqlBackend) IO ()
insertIntoImageTags :: ImageId -> [TagId] -> SqlReaderT ()
insertIntoImageTags imageId tags = do
      let imageTagEntities = map (\tagId -> ImageTag imageId tagId) tags
      runDB $ insertMany_ imageTagEntities



insertImageWithTags :: String -> Maybe String -> B.ByteString -> [String] -> SqlReaderT (Entity Image, [Entity Tag])
insertImageWithTags label url content tagNames = do
  tags <- insertTagsIfNotExist tagNames
  imgKey <- runDB $ insert $ Image label url content
  let imageId = fromSqlKey imgKey
  insertIntoImageTags imgKey tags
  head <$> (getByImageId $ ImageID imageId)


groupImagesAndMaybeTags :: [(Entity Image, Maybe b)] -> [(Entity Image, [b])]
groupImagesAndMaybeTags lst =  map  accumulate $ toGroups lst
                    where  toGroups = groupOn (entityKey . fst)

toGroups :: SqlReaderT [(Entity Image, Maybe(Entity Tag))] -> SqlReaderT [(Entity Image, [Entity Tag])]
toGroups lst = (liftM groupImagesAndMaybeTags) lst
--
--toGroups :: IO [(Entity Image, Maybe(Entity Tag))] -> IO [(Entity Image, [Entity Tag])]
--toGroups lst = (liftM groupImagesAndMaybeTags) lst

getId :: Entity Image -> ImageID
getId img = ImageID $ fromSqlKey $ entityKey img

accumulate :: [(a, Maybe b)] -> (a, [b])
accumulate =  (\xs -> (fst (head xs), catMaybes (map snd xs)))


mkEntity idNum value = Entity idNum value