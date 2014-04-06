module Handler.DelFromPortfolio where

import Import
import Data.Time
import qualified Data.Text as T (unpack)

getDelFromPortfolioR :: Text -> Text -> Handler Html
getDelFromPortfolioR isin date = do
  let time = read (T.unpack date) :: UTCTime
  _ <- runDB $ deleteBy $ OrderID isin time
  redirect PortfolioR
