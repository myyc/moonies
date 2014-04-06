module Handler.PortfolioActions where

import Import
import Handler.Variables
import Data.Time (UTCTime)
import qualified Data.Text as T (unpack)

data AddResult = AddResult
                 { resIsin :: Text
                 , resName :: Text
                 , resAbbr :: Text
                 , resQuotes :: Double
                 , resEurs :: Double
                 , resPrice :: Double
                 , resCurr :: Text
                 , resDate :: Text
                 } deriving (Show)

getAbbrForIsinR :: Text -> Handler Import.Value
getAbbrForIsinR isin = do
  md <- runDB $ getBy $ MDIsin isin
  let doc = case md of
        Nothing -> Metadata "" "" "" ""
        Just (Entity _ d) -> d
  return $ toJSON doc

getDelFromPortfolioR :: Text -> Text -> Handler Html
getDelFromPortfolioR isin date = do
  let time = read (T.unpack date) :: UTCTime
  _ <- runDB $ deleteBy $ OrderID isin time
  redirect PortfolioR

getAddToPortfolioR :: Handler Html
getAddToPortfolioR = defaultLayout $ do
  setTitle "Add"
  headWidget
  $(widgetFile "portfolio")
  toWidgetBody [hamlet|
        <div class="container" id="maincont">
           <div class="row frow">
              <div class="col-lg-11">
              <div class="col-lg-1 fcol">
                 <a href="/portfolio" class="rlink">X
           <form id="fmf" action="@{AddToPortfolioR}" method="post">
              <div class="row frow">
                 <div class="col-lg-3 fcol">ISIN
                 <div class="col-lg-7 fcol">
                    <input class="fform finvalid" type="text" name="isin" id="isinfield">
              <div class="row frow">
                 <div class="col-lg-3 fcol">Name
                 <div class="col-lg-7 fcol">
                    <input class="fform fstring finvalid fnabbr" type="text" name="name" id="namefield">
              <div class="row frow">
                 <div class="col-lg-3 fcol">Abbreviation
                 <div class="col-lg-7 fcol">
                    <input class="fform fstring finvalid fnabbr" type="text" name="abbr" id="abbrfield">
              <div class="row frow" style="margin-top: 20pt">
                 <div class="col-lg-3 fcol">Quotes
                 <div class="col-lg-7 fcol">
                    <input class="fform fnum finvalid" type="text" name="quotes">
              <div class="row frow">
                 <div class="col-lg-3 fcol">Eurs
                 <div class="col-lg-7 fcol">
                    <input class="fform fnum finvalid" type="text" name="eurs">
              <div class="row frow">
                 <div class="col-lg-3 fcol">Original Price
                 <div class="col-lg-7 fcol">
                    <input class="fform fnum finvalid" type="text" name="oprice">
              <div class="row frow">
                 <div class="col-lg-3 fcol">Currency
                 <div class="col-lg-7 fcol">
                    <input class="fform fstring finvalid" type="text" name="curr">
              <div class="row frow">
                 <div class="col-lg-3 fcol">Date
                 <div class="col-lg-7 fcol">
                    <input class="fform fdate finvalid" type="text" name="date">
              <div class="row frow" id="fsubb">
                 <div class="col-lg-11 fcol">
                 <div class="col-lg-1 fcol" style="margin-top: 18pt">
                    <a href="#" class="rlink">O
            |]
