module Handler.Graph (postGraphR) where

import Import

import Data.Aeson (encode)

import Data (storeMaster)

import qualified Graph

postGraphR :: Handler Value
postGraphR = do
  store <- appStore <$> getYesod
  ev    <- storeMaster store
  case ev of
    Left errors -> error $ show errors
    Right viewer -> do
      body  <- requireJsonBody
      value <- Graph.query store viewer body
      $(logInfo) $ "GraphQL response: " <> (toStrict . decodeUtf8 $ encode value)
      return value
