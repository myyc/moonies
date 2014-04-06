module Handler.AddToPortfolio where

import Import
import Handler.Variables

getAddToPortfolioR :: Handler Html
getAddToPortfolioR = defaultLayout $ do
  setTitle "Add"
  headWidget
  $(widgetFile "portfolio")
  toWidgetBody
     [hamlet|
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
              <div class="row frow">
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
