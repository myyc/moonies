module Handler.AmIRich where

import Import
import Handler.Variables

getAmIRichR :: Text -> Handler Html
getAmIRichR curr = defaultLayout $ do
  setTitle "Am I rich?"
  headWidget
  $(widgetFile "amirich")
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
