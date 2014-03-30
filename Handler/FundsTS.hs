{-# LANGUAGE DeriveGeneric #-}

module Handler.FundsTS where

import Import
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (addDays)
import Gatherers.Morningstar
import Gatherers.MongoDB

import GHC.Generics (Generic)

data TS = TS {isin :: Text, abbr :: Text, xys :: [XY]} deriving (Show, Generic)
instance FromJSON TS
instance ToJSON TS

getFundsTSR :: Text -> Handler Import.Value
getFundsTSR curr = do
  isins <- liftIO getIsins
  let fetchTS isin' = do
        ct <- liftIO getCurrentTime
        let today = utctDay ct
        assets <- getAssets isin'
        abbr' <- getAbbr isin'
        xys' <- getMorningStarValues curr isin' (addDays (-60) today) today
        return $ TS isin' abbr' (map (\xy -> XY (x xy) (y xy * weig assets)) xys')
  tss <- liftIO $ mapM fetchTS isins
  return $ toJSON tss
