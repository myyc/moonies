module Handler.Monies where

import Import
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (addDays)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Text as T
import Network.HTTP as H (simpleHTTP, getRequest, getResponseBody)
import Data.Maybe (catMaybes)
import Prelude (init, tail, head)
import Gatherers.Morningstar
import Gatherers.MongoDB

getMoniesR :: Text -> Handler Import.Value
getMoniesR curr = do
  isins <- liftIO getIsins
  a <- liftIO $ mapM (getLastValues curr) isins
  return $ toJSON (show a)

tuplify :: [Float] -> (Integer, Float)
tuplify [] = (0, 0)
tuplify [x] = (0, x)
tuplify a = (round $ head a, a !! 1)

getLastValues :: Text -> Text -> IO [(Integer, Float)]
getLastValues curr isin = do
  ct <- liftIO getCurrentTime
  let today = utctDay ct
      endDate = formatTime defaultTimeLocale "%F" today
      startDate = formatTime defaultTimeLocale "%F" (addDays (-7) today)
      currency = T.unpack curr
      url = getMorningStarURL currency startDate endDate (T.unpack isin)
  res <- liftIO $ simpleHTTP (H.getRequest url)
  body <- liftIO $ getResponseBody res
  liftIO $ putStrLn url
  let a = read body :: [[Float]]
      b = map tuplify a
  return b
