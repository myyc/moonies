module Handler.Variables where

import Import

red :: String
red = "#c60f13"

green :: String
green = "#4ca11c"

darkGrey :: String
darkGrey = "#1a1a1a"

lightGrey :: String
lightGrey = "#aaaaaa"

headWidget :: Widget
headWidget = do
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap.min.css"
  addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/css/bootstrap-theme.min.css"
  addScriptRemote "//code.jquery.com/jquery.js"
  addScriptRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.1/js/bootstrap.min.js"
  addScriptRemote "//cdnjs.cloudflare.com/ajax/libs/underscore.js/1.6.0/underscore-min.js"
  addScript $ StaticR js_overlay_js
  toWidget [cassius|
              body
                background: #{darkGrey}
                color: #{lightGrey}
              .tooltip-inner
                color: #{lightGrey}
                padding: 5px
                font-size: 20px
                font-weight: bold
           |]
