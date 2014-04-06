{-# LANGUAGE DeriveGeneric #-}
module Handler.Funds where

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

headWidget :: Text -> Widget
headWidget curr = do
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css"
  addStylesheet $ StaticR css_nv_d3_css
  addScriptRemote "//code.jquery.com/jquery.js"
  addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"
  addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.6.0/underscore-min.js"
  addScriptRemote "//d3js.org/d3.v3.min.js"
  addScript $ StaticR js_nv_d3_min_js
  addScript $ StaticR js_overlay_js
  $(widgetFile "funds")


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

getFundsR :: Text -> Handler Html
getFundsR curr = defaultLayout $ do
  setTitle "Charts"
  headWidget curr
  toWidgetBody
     [hamlet|
        <header id="head">
           <div class="container">
              <h1 id="htitle">Charts
        <div class="container">
           <div id="chart">
              <svg style="height:500px">
            |]
