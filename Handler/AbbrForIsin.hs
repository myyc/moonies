module Handler.AbbrForIsin where

import Import

getAbbrForIsinR :: Text -> Handler Import.Value
getAbbrForIsinR isin = do
  md <- runDB $ getBy $ MDIsin isin
  let doc = case md of
        Nothing -> Metadata "" "" "" ""
        Just (Entity _ d) -> d
  return $ toJSON doc
