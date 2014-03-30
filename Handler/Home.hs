module Handler.Home where

import Import
import Handler.AmIRich

getHomeR :: Handler Html
getHomeR = getAmIRichR "EUR"
