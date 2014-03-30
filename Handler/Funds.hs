module Handler.Funds where

import Import

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
