module Handler.Portfolio where

import Import
import Handler.Variables
import Gatherers.MongoDB (getRows, Row, rabbr, reurs, rquotes, rdate, rname, risin)

headWidget :: Widget
headWidget = do
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css"
  addScriptRemote "//code.jquery.com/jquery.js"
  addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"
  addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.6.0/underscore-min.js"
  addScript $ StaticR js_overlay_js
  $(widgetFile "commons")
  $(widgetFile "portfolio")

getPortfolioR :: Handler Html
getPortfolioR = defaultLayout $ do
  rows <- liftIO getRows
  setTitle "Portfolio"
  headWidget
  toWidgetBody
     [hamlet|
        <div class="container" id="maincont">
           $forall row <- rows
              <div class="row frow">
                 <div class="col-lg-4 fcol fttp" data-original-title="#{rname row}">#{rabbr row}
                 <div class="col-lg-2 fcol">#{reurs row} €
                 <div class="col-lg-1 fcol">#{rquotes row}
                 <div class="col-lg-4 fcol">#{show $ rdate row}
                 <div class="col-lg-1 fcol">
                    <a href="/portfolio/delete/#{risin row}/#{show $ rdate row}" class="rlink" id="dellink">X
           <div class="row frow">
              <div class="col-lg-4 fcol">
              <div class="col-lg-2 fcol">
              <div class="col-lg-1 fcol">
              <div class="col-lg-4 fcol">
              <div class="col-lg-1 fcol">
                 <a href="#" class="rlink" id="addlink">O
            |]
