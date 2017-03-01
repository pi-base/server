module Import
    ( module Import
    ) where

import Foundation            as Import
import Import.NoFoundation   as Import

getSetting :: (AppSettings -> a) -> Handler a
getSetting accessor = do
  settings <- appSettings <$> getYesod
  return $ accessor settings
