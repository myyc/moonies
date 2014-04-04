{-# LANGUAGE DeriveGeneric #-}

module Handler.ListFunds where

import Import
import Handler.Variables
import Gatherers.MongoDB (getRows)

import GHC.Generics (Generic)

headWidget :: Widget
headWidget = do
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css"
  addScriptRemote "//code.jquery.com/jquery.js"
  addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"
  addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.6.0/underscore-min.js"
  addScript $ StaticR js_overlay_js
  $(widgetFile "commons")
  $(widgetFile "listfunds")

getListFundsR' :: Handler Html
getListFundsR' = defaultLayout $ do
  rows' <- liftIO getRows
  setTitle "Funds"
  headWidget
  toWidgetBody
     [hamlet|
        <div class="container" id="maincont">
           <div class="row">–</div>
            |]

getListFundsR :: Handler Import.Value
getListFundsR = do
  rows <- liftIO getRows
  return $ toJSON rows
