{-# LANGUAGE DeriveGeneric #-}

module Gatherers.MongoDB where

import Import
import Database.MongoDB
import Data.Maybe (catMaybes, isNothing, fromJust, fromMaybe)
--import Data.List (head)
import qualified Data.Text as T

import GHC.Generics (Generic)

data Figures = Figures {weig :: Double, eur :: Double, orig :: Double} deriving (Show, Generic)
instance FromJSON Figures
instance ToJSON Figures

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
  pipe <- liftIO $ runIOE $ connect $ host "127.0.0.1"
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
  pipe <- liftIO $ runIOE $ connect $ host "127.0.0.1"
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
  pipe <- liftIO $ runIOE $ connect $ host "127.0.0.1"
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
