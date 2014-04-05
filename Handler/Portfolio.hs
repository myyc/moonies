module Handler.Portfolio where

import Import
import Handler.Variables
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Time (UTCTime)

data Row = Row { risin :: Text
               , rname :: Text
               , rabbr :: Text
               , reurs :: Double
               , rquotes :: Double
               , rdate :: UTCTime
               } deriving (Show)

getRows :: Handler [Row]
getRows = do
  mdlist <- runDB $ selectList [] [Asc MetadataIsin] >>= mapM (\(Entity _ r) -> return (metadataIsin r, r))
  let mdmap = M.fromList mdlist
      md i = fromMaybe (Metadata "" "" "" "") (M.lookup i mdmap)
      rowify (Entity _ r) = return $ Row (orderIsin r) (name r) (abbr r) (orderJewgolds r) (orderQuotes r) (orderDate r)
      name = metadataName . md . orderIsin
      abbr = metadataAbbr . md . orderIsin
  runDB $ selectList [] [Asc OrderIsin] >>= mapM rowify

getPortfolioR :: Handler Html
getPortfolioR = defaultLayout $ do
  rows <- liftHandlerT getRows
  setTitle "Portfolio"
  headWidget
  $(widgetFile "portfolio")
  toWidgetBody
     [hamlet|
        <div class="container" id="maincont">
           $forall row <- rows
              <div class="row frow">
                 <div class="col-lg-4 fc fcol fttp" data-original-title="#{rname row}">#{rabbr row}
                 <div class="col-lg-2 fc fcol">#{reurs row} €
                 <div class="col-lg-1 fc fcol">#{rquotes row}
                 <div class="col-lg-4 fc fcol">#{show $ rdate row}
                 <div class="col-lg-1 fc fcol">
                    <a href="@{DelFromPortfolioR (risin row) ((T.pack . show) $ rdate row)}" class="rlink" id="dellink">X
           <div class="row frow">
              <div class="col-lg-4 fc fcol">
              <div class="col-lg-2 fc fcol">
              <div class="col-lg-1 fc fcol">
              <div class="col-lg-4 fc fcol">
              <div class="col-lg-1 fc fcol">
                 <a href="/portfolio/add" class="rlink" id="addlink">O
            |]
