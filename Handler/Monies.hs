module Handler.Monies where

import Import
import Data.Time.Clock (utctDay, getCurrentTime)
import Data.Time.Calendar (addDays)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Text as T
import Network.HTTP as H (simpleHTTP, getRequest, getResponseBody)
import Database.MongoDB
import Data.Maybe (catMaybes)
import Data.Configurator
import Prelude (init, tail)

getMorningStarURL :: String -> String -> String -> String -> IO String
getMorningStarURL currency startDate endDate isin = do
  cfg <- liftIO $ load [Required "conf/default.conf"]
  url <- liftIO $ require cfg "morningstar.url"
  return $ url ++ "?idType=isin&frequency=hourly" \
  ++ "&priceType=&outputType=COMPACTJSON&currencyId=" ++ currency ++ \
    "&startDate=" ++ startDate ++ "&endDate=" ++ endDate ++ "&id=" ++ isin

getMoniesR :: Text -> Handler Import.Value
getMoniesR curr = do
  pipe <- liftIO $ runIOE $ connect $ host "127.0.0.1"
  e <- liftIO $ access pipe master "funds" $ do
    find (select [] "private") >>= rest
  liftIO $ close pipe
  let f = case e of
        Left _ -> []
        Right ds -> do
          map (\d -> (cast' (valueAt "isin" d)) :: Maybe Text) ds
  a <- liftIO $ mapM (getLastValues curr) (catMaybes f)
  return $ toJSON (show a)

tuplify :: [Float] -> (Integer, Float)
tuplify [] = (0, 0)
tuplify [x] = (0, x)
tuplify a = (round $ a !! 0, a !! 1)

getLastValues :: Text -> Text -> IO [(Integer, Float)]
getLastValues curr isin = do
  ct <- liftIO getCurrentTime
  let today = utctDay ct
      endDate = formatTime defaultTimeLocale "%F" (today)
      startDate = formatTime defaultTimeLocale "%F" (addDays (-7) today)
      currency = T.unpack curr
  url <- getMorningStarURL currency startDate endDate (T.unpack isin)
  res <- liftIO $ simpleHTTP (H.getRequest url)
  body <- liftIO $ getResponseBody res
  liftIO $ putStrLn url
  let a = read body :: [[Float]]
      b = map tuplify a
  return $ b
