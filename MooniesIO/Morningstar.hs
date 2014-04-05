{-# LANGUAGE DeriveGeneric #-}

module MooniesIO.Morningstar where

import Import
import MooniesIO.Private

import Data.Time.Format (formatTime, FormatTime)
import System.Locale (defaultTimeLocale)
import qualified Data.Text as T
import Network.HTTP as H (simpleHTTP, getRequest, getResponseBody)
import Prelude (init, tail, head)
import MooniesIO.MongoDB (getCachedTS, cacheTS)

import GHC.Generics (Generic)

data XY = XY {x :: Double, y :: Double} deriving (Show, Generic)
instance FromJSON XY
instance ToJSON XY

xyify :: [Double] -> XY
xyify l = case l of
  []  -> XY 0 0
  [u] -> XY 0 u
  v   -> XY (head v / 1000) (v !! 1)

getMorningStarURL :: String -> String -> String -> String -> String
getMorningStarURL currency startDate endDate isin = url
  where url = baseMorningstarURL ++ "?idType=isin&frequency=hourly"
          ++ "&priceType=&outputType=COMPACTJSON&currencyId=" ++ currency
          ++ "&startDate=" ++ startDate ++ "&endDate=" ++ endDate ++ "&id="
          ++ isin

getMorningStarValues :: FormatTime t => Text -> Text -> t -> t -> IO [XY]
getMorningStarValues curr isin startDate endDate = do
  let endDateStr = formatTime defaultTimeLocale "%F" endDate
      startDateStr = formatTime defaultTimeLocale "%F" startDate
      currency = T.unpack curr
      url = getMorningStarURL currency startDateStr endDateStr (T.unpack isin)
  cachedTS <- getCachedTS $ T.pack url
  case cachedTS of
        [[]] -> do
          res <- liftIO $ simpleHTTP (H.getRequest url)
          body <- liftIO $ getResponseBody res
          liftIO $ putStrLn url
          let a = read body :: [[Double]]
          liftIO $ cacheTS (T.pack url) a
          return $ map xyify a
        v -> return $ map xyify v
