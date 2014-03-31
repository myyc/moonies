{-# LANGUAGE DeriveGeneric #-}

module Handler.Monies where

import Import
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (addDays)
import Data.List (last)
import Gatherers.Morningstar
import Gatherers.MongoDB
import Control.Concurrent (forkIO)

import GHC.Generics (Generic)

data Monies = Monies {currsum :: Double, origsum :: Double, spec :: [AbbRet]} deriving (Show, Generic)
instance FromJSON Monies
instance ToJSON Monies

data AbbRet = AbbRet {isin :: Text, abbr :: Text, ret :: Double, price :: Double} deriving (Show, Generic)
instance FromJSON AbbRet
instance ToJSON AbbRet

getMoniesR :: Text -> Handler Import.Value
getMoniesR curr = do
  isins <- liftIO getIsins
  let getAbbrAndRet isin'' = do
        asset <- getAssets isin''
        abbr' <- getAbbr isin''
        curr' <- getCurr isin''
        price' <- getLastPrice curr' isin''
        let bef = orig asset
            aft = weig asset * price'
        return $ AbbRet isin'' abbr' ((aft-bef)/aft) price'
  let getSum isin'' = do
        asset <- getAssets isin''
        price' <- getLastPrice curr isin''
        return $ weig asset * price'
  a <- liftIO $ mapM getAbbrAndRet isins
  b <- liftIO $ mapM getSum isins
  origsum' <- liftIO getOrigSum
  return $ toJSON $ Monies (sum b) origsum' a

getLastPrice :: Text -> Text -> IO Double
getLastPrice curr isin' = do
  ct <- liftIO getCurrentTime
  let today = utctDay ct
  v <- getMorningStarValues curr isin' (addDays (-7) today) today
  (return . y . last) v
