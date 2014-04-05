{-# LANGUAGE DeriveGeneric #-}

module Handler.Monies where

import Import
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (addDays)
import Data.List (last)
import MooniesIO.Morningstar
import MooniesIO.MongoDB

import GHC.Generics (Generic)

data Monies = Monies {currsum :: Double, origsum :: Double, spec :: [AbbRet]} deriving (Show, Generic)
instance FromJSON Monies
instance ToJSON Monies

data AbbRet = AbbRet {isin :: Text, abbr :: Text, ret :: Double, price :: Double} deriving (Show, Generic)
instance FromJSON AbbRet
instance ToJSON AbbRet

getMoniesR :: Text -> Handler Import.Value
getMoniesR curr = do
  stuff <- getJoined
  let getAbbrAndRet (order, md) = do
        let isin' = orderIsin order
        asset <- getAssets isin'
        price' <- liftIO $ getLastPrice (metadataCurrency md) isin'
        let bef = orig asset
            aft = weig asset * price'
        return $ AbbRet isin' (metadataAbbr md) ((aft-bef)/aft) price'
  let getSum (order, _) = do
        let isin' = orderIsin order
        asset <- getAssets isin'
        price' <- liftIO $ getLastPrice curr isin'
        return $ weig asset * price'
  abbrets <- mapM getAbbrAndRet stuff
  o <- mapM (\(order, md) -> return $ orderJewgolds order) stuff
  c <- mapM getSum stuff
  return $ toJSON $ Monies (sum c) (sum o) abbrets

getLastPrice :: Text -> Text -> IO Double
getLastPrice curr isin' = do
  ct <- liftIO getCurrentTime
  let today = utctDay ct
  v <- getMorningStarValues curr isin' (addDays (-7) today) today
  (return . y . last) v
