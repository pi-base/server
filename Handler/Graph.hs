module Handler.Graph (postGraphR) where

import Import

import Data.Aeson (encode)

import Data (storeMaster)

import qualified Graph

postGraphR :: Handler Value
postGraphR = do
  store <- appStore <$> getYesod
  body  <- requireJsonBody
  value <- Graph.query store body
  $(logDebug) $ "[GraphQL] " <> (toStrict . decodeUtf8 $ encode value)
  return value
