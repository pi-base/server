{-# LANGUAGE TemplateHaskell #-}
module Handler.Graph 
  ( postGraphR
  ) where

import Import
import Control.Lens    hiding ((.=))
import Data.Aeson.Lens
import Graph.Root      (asJSON, compiled, interpreted)
import Graph.Schema    (QueryData)
import GraphQL         as GQL (Response)
import Handler.Errors  (withErrorHandling)

postGraphR :: Handler Value
postGraphR = withErrorHandling $ do
  body <- requireJsonBody
  $(logInfo)  $ "[GraphQL] " <> (body ^. key "operationName" . _String)
  $(logDebug) $ "[GraphQL] " <> pj (body ^. key "variables" . _Object)

  settings <- appSettings <$> getYesod
  response <- asJSON (execute settings) body
  $(logDebug) $ "[GraphQL] " <> pj response

  sendStatusJSON status200 response

execute :: AppSettings -> QueryData -> Handler GQL.Response
execute s@AppSettings{..} q = if appCompileQueries
  then do
    cache <- appQueryCache <$> getYesod
    compiled s cache q
  else interpreted s q