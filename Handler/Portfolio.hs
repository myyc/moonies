module Handler.Portfolio where

import Import
import Handler.Variables
import Gatherers.MongoDB (getRows, Row, rabbr, reurs, rquotes, rdate, rname, risin)

getPortfolioR :: Handler Html
getPortfolioR = defaultLayout $ do
  rows <- liftIO getRows
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
                    <a href="/portfolio/delete/#{risin row}/#{show $ rdate row}" class="rlink" id="dellink">X
           <div class="row frow">
              <div class="col-lg-4 fc fcol">
              <div class="col-lg-2 fc fcol">
              <div class="col-lg-1 fc fcol">
              <div class="col-lg-4 fc fcol">
              <div class="col-lg-1 fc fcol">
                 <a href="/portfolio/add" class="rlink" id="addlink">O
            |]
