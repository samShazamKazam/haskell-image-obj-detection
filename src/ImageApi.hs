{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ImageApi
    ( startApp, AppCtx(..)
    ) where

import qualified ImageSqlBackend as Dao
import Data.Aeson as J
import qualified Data.Text as T
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.Multipart
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS (ByteString)
import Control.Monad.IO.Class
import Database.Persist
import Data.Char (isSpace)
import qualified GoogleVision as GV
import Data.List (nub)
import qualified Data.Text.Encoding as E
import GHC.Generics (Generic)
import Data.Char (toLower)
import ImageUrl
import Control.Monad (liftM)
import Control.Monad.Trans.Reader (runReaderT, ReaderT(..), asks, ask)
import ConfigReader
import Data.Pool (Pool)
import Database.Persist.Sql (SqlBackend)

data MetaData = MetaData
    { imageId :: Int,
      label :: String,
      url :: Maybe String,
      objects  :: [String]
    } deriving (Eq, Show, Generic)

data Content = Content BS.ByteString
    deriving (Eq, Show, Generic)

data Image = Image { content :: Content, metaData :: MetaData }
       deriving (Eq, Show, Generic)


instance FromJSON Content  where
  parseJSON = withText "Content" $ \t -> pure (Content (E.encodeUtf8 t))

instance FromJSON MetaData
instance FromJSON Image

instance ToJSON MetaData
instance ToJSON Content
instance ToJSON Image


type ImageId = Int
type Label = String
type Url = String
type Tag = String
type ImageContent = BS.ByteString

type ImageAPI = "images" :>  QueryParam "objects" String :> Get '[JSON] [Image]
                :<|> "images" :> Capture "imageId" Int :> Get '[JSON] Image
                :<|> "images" :> MultipartForm Mem (MultipartData Mem) :> Post '[JSON] Image

server :: ServerT ImageAPI AppM
server =  getAllImages
        :<|> getImage
        :<|> uploadImageData

-------------------------------------------------
mkMetaData :: Int -> String -> Maybe String -> [String] -> MetaData
mkMetaData imgId label url objects = MetaData imgId label url objects

mkContent :: BS.ByteString -> Content
mkContent content = Content content

mkImage content metaData = Image content metaData



toImage :: (Entity Dao.Image, [Entity Dao.Tag]) -> Image
toImage (img, tags) = mkImage content md
        where
            idnum =  (fromIntegral . Dao.getImageId . Dao.getId)
            lbl = (Dao.imageLabel.entityVal)
            url = (Dao.imageUrl . entityVal)
            tagf = (Dao.tagName. entityVal)
            md = mkMetaData (idnum img) (lbl img) (url img) (map tagf tags)
            content = (mkContent . Dao.imageContent . entityVal) img

-------------------------------------------------
-- | Given a list of objects, find all the images that have associations with any of them

getAllImages :: Maybe String ->  AppM [Image]
getAllImages Nothing =  getAllImages_
getAllImages (Just str) = do
     cfg <- ask
     let objs = (map toLower) <$> filteredList
         pool = connPool cfg
     liftIO $ map toImage <$> runReaderT (Dao.getImagesWithObj objs) pool
     where filteredList = filter ((not.blank).trim) (split (removeQuotes str))


getAllImages_ :: AppM [Image]
getAllImages_ = do
     cfg <- ask
     let pool = connPool cfg
     liftIO $ map toImage <$> runReaderT (Dao.getAllImagesWithTags) pool


removeQuotes :: String -> String
removeQuotes ('"':xs) | last xs == '"' = init xs
removeQuotes xs = xs

trim :: String -> String
trim = unwords . words

blank :: [Char] -> Bool
blank = all isSpace

split :: String -> [String]
split str = case break (==',') str of
                (a, _comma:b) -> a : split b
                (a, _empty)   -> [a]

-----------------------------------------------

getImage_ :: ImageId -> AppM (Maybe [Image])
getImage_ n = do
        cfg <- ask
        let pool = connPool cfg
        v <- liftIO $ runReaderT (Dao.getByImageId (Dao.mkImageId (fromIntegral  n))) pool
        case v of
            [] -> return $ Nothing
            xs:_ -> return $ Just $ map toImage [xs]



getImage :: ImageId -> AppM Image
getImage n = do
    v <- getImage_ n
    case v of
        Nothing -> throwError err404 { errBody = "Image not found" }
        Just xs -> return $ head xs

--------------------------------------------------

-----------------------------------------------------------

getLabel :: MultipartData Mem -> Label
getLabel multipartData = do
    case (lookupInput "label" multipartData) of
        Left _  -> ""
        Right b -> T.unpack b

getEnableDetection :: MultipartData Mem -> Bool
getEnableDetection multipartData = do
    case (lookupInput "detect_objects" multipartData) of
        Left _  -> False
        Right _ -> True

getUrl :: MultipartData Mem -> Maybe Url
getUrl multipartData = do
    case (lookupInput "url" multipartData) of
        Left _  -> Nothing
        Right url -> Just $ T.unpack url


removeDuplicates :: Eq a => [a] -> [a]
removeDuplicates xs = nub xs

type DetectFunc = (BS.ByteString -> IO(Either GV.GoogleError [String]))

detectObjects_ :: DetectFunc -> ImageContent -> AppM (Either GV.GoogleError [Tag])
detectObjects_ detect content = do
    eitherErrTags <- liftIO $ detect content
    case eitherErrTags of
        Left err -> return $ Left err
        Right tags -> return $ Right $ removeDuplicates tags


detectObjects :: Bool -> DetectFunc -> ImageContent -> AppM (Either GV.GoogleError [Tag])
detectObjects shouldDetect detect content = do
        if shouldDetect then detectObjects_ detect content
        else return $ Right []


detectObjectsWithContent :: Label -> Maybe Url -> Bool -> DetectFunc -> ImageContent -> AppM (Either GV.GoogleError Image)
detectObjectsWithContent label url shouldDetect detect content = do
        eitherErrTags <- detectObjects shouldDetect detect content
        case eitherErrTags of
            Left err -> return $ Left err
            Right tags -> do
                cfg <- ask
                let tags_ = (map toLower) <$> tags
                    pool = connPool cfg
                imgWithTags <- liftIO $ runReaderT (Dao.insertImageWithTags label url content tags_) pool
                return $ Right $ toImage imgWithTags


uploadImageData :: MultipartData Mem -> AppM Image
uploadImageData multipartData = do
        let label = getLabel multipartData
        let shouldDetect = getEnableDetection multipartData
        let maybeurl = getUrl multipartData
        cfg <- ask
        let key = apiKey $ googleVision $ svcCfg cfg
        let detectF = GV.getLabelsForImageContent key
        case maybeurl of
            Nothing -> do
                case files multipartData of
                    [] -> throwError err400 { errBody = "Neither URL nor a file were given" }
                    file:_ -> do
                        eitherErrId <- do
                            let content = LBS.toStrict $ fdPayload file
                            detectObjectsWithContent label maybeurl shouldDetect detectF content
                        case eitherErrId of
                            Left _ -> throwError err500 { errBody = "The service cant detect objects images right now. Try later" }
                            Right img -> return $ img
            Just url -> do
                eitherImg <- liftIO $ fetchImage url
                case eitherImg of
                    Left err -> do
                            liftIO $ putStrLn $ "error = " ++ err
                            throwError err400 { errBody = "URL does not refer to an image" }
                    Right content -> do
                        eitherErrId <- detectObjectsWithContent label maybeurl shouldDetect detectF (LBS.toStrict content)
                        case eitherErrId of
                            Left _ -> throwError err500 { errBody = "The service cant detect objects images right now. Try later" }
                            Right img -> return $ img


startApp :: AppCtx -> IO ()
startApp ctx = run 8080 (mkApp ctx)

api :: Proxy ImageAPI
api = Proxy

mkApp :: AppCtx -> Application
mkApp appCtx = serveWithContext api (appCtx :. EmptyContext) (hoistServerWithContext api (Proxy :: Proxy '[AppCtx]) (nt appCtx) server)

nt :: AppCtx -> AppM a -> Handler a
nt appCtx appM = runReaderT appM appCtx


data Error = HttpError {code :: Int, msg :: String} | OtherError {msg :: String}
