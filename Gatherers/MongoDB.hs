{-# LANGUAGE DeriveGeneric #-}

module Gatherers.MongoDB where

import Import
import Database.MongoDB
import Data.Bson as B (lookup)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time (getCurrentTime, diffUTCTime, UTCTime)
import qualified Data.Text as T
import qualified Data.Map as M

import GHC.Generics (Generic)

-- types

data Figures = Figures { weig :: Double
                       , eur :: Double
                       , orig :: Double} deriving (Show, Generic)

instance FromJSON Figures
instance ToJSON Figures

justFiguresify :: Maybe Double -> Maybe Double -> Maybe Double -> Maybe Figures
justFiguresify w e o = Figures <$> w <*> e <*> o

data Metadata = Metadata { mname :: String
                         , mabbr :: String
                         , mcurr :: String
                         } deriving (Show)

justMDify :: Maybe String -> Maybe String -> Maybe String -> Maybe Metadata
justMDify n a c = Metadata <$> n <*> a <*> c

emptyMD :: Metadata
emptyMD = Metadata "" "" ""

data Row = Row { risin :: Text
               , rname :: String
               , rabbr :: String
               , reurs :: Double
               , rquotes :: Double
               , rdate :: UTCTime
               } deriving (Show, Generic)

instance FromJSON Row
instance ToJSON Row

justRowify :: Maybe Text -> Maybe String -> Maybe String -> Maybe Double -> Maybe Double -> Maybe UTCTime -> Maybe Row
justRowify i n a e q d = Row <$> i <*> n <*> a <*> e <*> q <*> d

-- end types

getConnection :: IO Pipe
getConnection = (runIOE . connect . host) "127.0.0.1"

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
          where getWeig d = B.lookup "quotes" d :: Maybe Double
                getEur d = B.lookup "jewgolds" d :: Maybe Double
                getOrig d = B.lookup "origprice" d :: Maybe Double
      a = catMaybes f
  return $ foldl sumFigures (Figures 0 0 0) a
    where sumFigures u v = Figures (weig u + weig v) (eur u + eur v) (weig u * orig u + weig v * orig v)

getOneFromMD :: Text -> IO (Maybe Metadata)
getOneFromMD isin = do
  pipe <- liftIO getConnection
  doc <- access pipe master "funds" $
       findOne (select ["isin" =: isin] "metadata")
  return $ case doc of
    Left _ -> Nothing
    Right d -> case d of
      Nothing -> Nothing
      Just d' -> justMDify (getName' d') (getAbbr' d') (getCurr' d')
          where getName' m = B.lookup "name" m :: Maybe String
                getAbbr' m = B.lookup "abbr" m :: Maybe String
                getCurr' m = B.lookup "currency" m :: Maybe String

getAllFromMD :: IO (M.Map Text Metadata)
getAllFromMD = do
  pipe <- liftIO getConnection
  cur <- access pipe master "funds" $
       find (select [] "metadata") >>= rest
  return $ case cur of
    Left _ -> M.empty
    Right ds -> (M.fromList . catMaybes) (map tuplify ds)
      where tuplify d = case getIsin' d of
              Nothing -> Nothing
              Just i -> case justMDify (getName' d) (getAbbr' d) (getCurr' d) of
                Nothing -> Nothing
                Just m -> Just (i, m)
            getIsin' e = B.lookup "isin" e :: Maybe Text
            getName' m = B.lookup "name" m :: Maybe String
            getAbbr' m = B.lookup "abbr" m :: Maybe String
            getCurr' m = B.lookup "currency" m :: Maybe String

getAbbr :: Text -> IO Text
getAbbr isin = do
  m <- getOneFromMD isin
  return $ case m of
    Nothing -> T.pack ""
    Just a -> (T.pack . mabbr) a

getCurr :: Text -> IO Text
getCurr isin = do
  m <- getOneFromMD isin
  return $ case m of
    Nothing -> T.pack ""
    Just a -> (T.pack . mcurr) a

getRows :: IO [Row]
getRows = do
  mds <- getAllFromMD
  cur <- dumpPrivate
  return $ catMaybes $ case cur of
    Left _ -> []
    Right ds -> map rowify ds
      where rowify d = justRowify (getIsin' d) (Just $ mname $ md d) (Just $ mabbr $ md d) (getEurs' d) (getQuotes' d) (getDate' d)
            getIsin' e = B.lookup "isin" e :: Maybe Text
            getQuotes' m = B.lookup "quotes" m :: Maybe Double
            getEurs' m = B.lookup "jewgolds" m :: Maybe Double
            getDate' m = B.lookup "date" m :: Maybe UTCTime
            md d = case getIsin' d of
              Nothing -> emptyMD
              Just i -> fromMaybe emptyMD $ M.lookup i mds

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
