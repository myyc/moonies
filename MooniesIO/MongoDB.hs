{-# LANGUAGE DeriveGeneric #-}

module MooniesIO.MongoDB where

import Import
import Database.MongoDB
import Data.Maybe (fromMaybe)
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import qualified Data.Map as M

import GHC.Generics (Generic)

-- types

data Figures = Figures { weig :: Double
                       , eur :: Double
                       , orig :: Double} deriving (Show, Generic)

instance FromJSON Figures
instance ToJSON Figures

-- end types

getIsins :: Handler [Text]
getIsins = runDB $ selectList [] [Asc MetadataIsin] >>= mapM (\(Entity _ r) -> return $ metadataIsin r)

getJoined :: Handler [(Import.Order, Metadata)]
getJoined = do
  mdlist <- runDB $ selectList [] [Asc MetadataIsin] >>= mapM (\(Entity _ r) -> return (metadataIsin r, r))
  let mdmap = M.fromList mdlist
      md i = fromMaybe (Metadata "" "" "" "") (M.lookup i mdmap)
      rowify (Entity _ r) = return (r, md $ orderIsin r)
  runDB $ selectList [] [Asc OrderIsin] >>= mapM rowify

getAssets :: Text -> Handler Figures
getAssets isin = do
  let figuresify (Entity _ r) = return $ Figures (orderQuotes r) (orderJewgolds r) (orderOrigprice r)
  olist <- runDB $ selectList [OrderIsin ==. isin] [] >>= mapM figuresify
  return $ foldl sumFigures (Figures 0 0 0) olist
    where sumFigures u v = Figures (weig u + weig v) (eur u + eur v) (weig u * orig u + weig v * orig v)

getConnection :: IO Pipe
getConnection = (runIOE . connect . host) "127.0.0.1"

-- cache! it's shit, but it works now. until i expand the mining section.
validateDoc :: Document -> UTCTime -> Maybe [[Double]]
validateDoc d now = if valid d now then data' else Just [[]]
  where data' = cast' (valueAt "data" d) :: Maybe [[Double]]
        valid d' now' = case (cast' (valueAt "_id" d') :: Maybe ObjectId) of
          Nothing -> False
          Just oid -> diffUTCTime now' (timestamp oid) < 600

getCachedTS :: Text -> IO [[Double]]
getCachedTS url = do
  ct <- liftIO getCurrentTime
  pipe <- liftIO getConnection
  doc <- access pipe master "funds" $
       findOne (select ["url" =: url] "mscache") {sort = ["_id" =: (-1 :: Int)]}
  let f = case doc of
        Left _ -> Just [[]]
        Right d -> case d of
          Nothing -> Just [[]]
          Just d' -> validateDoc d' ct
      t = fromMaybe [[]] f
  return t

cacheTS :: Text -> [[Double]] -> IO ()
cacheTS url ts = do
  pipe <- liftIO getConnection
  res <- access pipe master "funds" $
         Database.MongoDB.insert "mscache" ["url" =: url, "data" =: ts]
  case res of
    Left _ -> return ()
    Right _ -> return ()

-- not used yet because I can't figure out how to trigger this in non-IO
uncacheTS :: Database.MongoDB.Value -> IO ()
uncacheTS oid = do
  pipe <- liftIO getConnection
  res <- access pipe master "funds" $
         Database.MongoDB.delete (select ["_id" := oid] "mscache")
  case res of
    Left _ -> return ()
    Right _ -> return ()
-- end of the cache functions
