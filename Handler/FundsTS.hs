{-# LANGUAGE DeriveGeneric #-}

module Handler.FundsTS where

import Import
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (addDays)
import MooniesIO.Morningstar
import MooniesIO.MongoDB
import Data.Maybe (fromMaybe)
import qualified Data.Map as M

import GHC.Generics (Generic)

data TS = TS {isin :: Text, abbr :: Text, xys :: [XY]} deriving (Show, Generic)
instance FromJSON TS
instance ToJSON TS

getFundsTSR :: Text -> Handler Import.Value
getFundsTSR curr = do
  isins <- getIsins
  mdmapl <- runDB $ selectList [] [Asc MetadataIsin] >>= mapM (\(Entity _ r) -> return (metadataIsin r, metadataAbbr r))
  let fetchTS isin' = do
        ct <- liftIO getCurrentTime
        let today = utctDay ct
            abbr' = fromMaybe "" $ M.lookup isin' mdmap
        assets <- getAssets isin'
        xys' <- liftIO $ getMorningStarValues curr isin' (addDays (-60) today) today
        return $ TS isin' abbr' (map (\xy -> XY (x xy) (y xy * weig assets)) xys')
      mdmap = M.fromList mdmapl
  tss <- mapM fetchTS isins
  return $ toJSON tss
