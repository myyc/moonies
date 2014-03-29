module Gatherers.MongoDB where

import Import
import Database.MongoDB
import Data.Maybe (catMaybes)

-- gets output from the funds.private collection.
-- sample doc:
-- {
--  "isin" : "AB1234567890", "quotes" : 6.66, "jewgolds" : 666, "origprice" : 66.6,
--  "date" : ISODate("2014-06-06T06:06:06Z")
-- }

dumpPrivate :: IO (Either Failure [Document])
dumpPrivate = do
  pipe <- liftIO $ runIOE $ connect $ host "127.0.0.1"
  e <- access pipe master "funds" $
       find (select [] "private") >>= rest
  liftIO $ close pipe
  return e

getIsins :: IO [Text]
getIsins = do
  cur <- dumpPrivate
  let f = case cur of
        Left _ -> []
        Right ds -> map (\d -> cast' (valueAt "isin" d) :: Maybe Text) ds
      a = catMaybes f
  return a
