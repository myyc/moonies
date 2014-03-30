module Handler.AmIRich where

import Import

headWidget :: Text -> Widget
headWidget curr = do
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css"
  addScriptRemote "//code.jquery.com/jquery.js"
  addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"
  addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.6.0/underscore-min.js"
  addScript $ StaticR js_overlay_js
  $(widgetFile "amirich")

getAmIRichR :: Text -> Handler Html
getAmIRichR curr = defaultLayout $ do
  setTitle "Am I rich?"
  headWidget curr
  toWidgetBody
     [hamlet|
        <div class="container" id="maincont">
           <div class="row main text-center">
              <span class="rg" id="bigtotal">
           <div class="row">
               <div class="col-lg-6 text-center" id="lcol">
                  <span class="rg perc" id="lperc">
               <div class="col-lg-6 text-center" id="rcol">
                  <ul id="rpercs">
            |]
