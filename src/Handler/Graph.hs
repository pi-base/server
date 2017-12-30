{-# LANGUAGE TemplateHaskell #-}
module Handler.Graph (postGraphR) where

import Import
import Graph.Root (asJSON, interpret)
import Util       (pj)

postGraphR :: Handler Value
postGraphR = do
  body <- requireJsonBody
  $(logDebug) $ "[GraphQL] " <> pj body
  response <- asJSON interpret body
  $(logDebug) $ "[GraphQL] " <> pj response -- (toStrict . decodeUtf8 $ encode value)
  return response
