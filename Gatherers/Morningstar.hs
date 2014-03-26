module Gatherers.Morningstar where

import Import
import Gatherers.Private

getMorningStarURL :: String -> String -> String -> String -> String
getMorningStarURL currency startDate endDate isin = url
  where url = baseMorningstarURL ++ "?idType=isin&frequency=hourly"
          ++ "&priceType=&outputType=COMPACTJSON&currencyId=" ++ currency
          ++ "&startDate=" ++ startDate ++ "&endDate=" ++ endDate ++ "&id="
          ++ isin
