{-# LANGUAGE DeriveGeneric #-}

module Gatherers.MongoDB where

import Import
import Database.MongoDB
import Data.Maybe (catMaybes, isNothing, fromJust, fromMaybe)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import qualified Data.Text as T

import GHC.Generics (Generic)

data Figures = Figures {weig :: Double, eur :: Double, orig :: Double} deriving (Show, Generic)
instance FromJSON Figures
instance ToJSON Figures

getConnection :: IO Pipe
getConnection = (runIOE . connect . host) "127.0.0.1"

justFiguresify :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Figures
justFiguresify w e o
  | isNothing w = Nothing
  | isNothing e = Nothing
  | isNothing o = Nothing
  | otherwise   = Just (Figures (fromJust w) (fromJust e) (fromJust o))

-- gets output from the funds.private collection.
-- sample doc:
-- {
--  "isin" : "AB1234567890", "quotes" : 6.66, "jewgolds" : 666, "origprice" : 66.6,
--  "date" : ISODate("2014-06-06T06:06:06Z")
-- }
dumpPrivate :: IO (Either Failure [Document])
dumpPrivate = do
  pipe <- liftIO getConnection
  e <- access pipe master "funds" $
       find (select [] "private") >>= rest
  liftIO $ close pipe
  return e

getIsins :: IO [Text]
getIsins = do
  cur <- dumpPrivate
  let f = case cur of
        Left _ -> []
        Right ds -> map (\d -> cast' (valueAt "isin" d) :: Maybe Text) ds
      a = catMaybes f
  return a

getOrigSum :: IO Double
getOrigSum = do
  cur <- dumpPrivate
  let f = case cur of
        Left _ -> []
        Right ds -> map (\d -> cast' (valueAt "jewgolds" d) :: Maybe Double) ds
      a = catMaybes f
  return $ sum a

getAssets :: Text -> IO Figures
getAssets isin = do
  pipe <- liftIO getConnection
  cur <- access pipe master "funds" $
       find (select ["isin" =: isin] "private") >>= rest
  liftIO $ close pipe
  let f = case cur of
        Left _ -> []
        Right ds -> map (\d -> justFiguresify (getWeig d) (getEur d) (getOrig d)) ds
          where getWeig d = cast' (valueAt "quotes" d) :: Maybe Double
                getEur d = cast' (valueAt "jewgolds" d) :: Maybe Double
                getOrig d = cast' (valueAt "origprice" d) :: Maybe Double
      a = catMaybes f
  return $ foldl sumFigures (Figures 0 0 0) a
    where sumFigures u v = Figures (weig u + weig v) (eur u + eur v) (weig u * orig u + weig v * orig v)

getFromMD :: Text -> Text -> IO Text
getFromMD isin key = do
  pipe <- liftIO getConnection
  doc <- access pipe master "funds" $
       findOne (select ["isin" =: isin] "metadata")
  let f = case doc of
        Left _ -> Just (T.pack "")
        Right d -> case d of
          Nothing -> Just (T.pack "")
          Just d' -> cast' (valueAt key d') :: Maybe Text
      t = fromMaybe (T.pack "") f
  return t

getAbbr :: Text -> IO Text
getAbbr isin = getFromMD isin "abbr"

getCurr :: Text -> IO Text
getCurr isin = getFromMD isin "currency"

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
