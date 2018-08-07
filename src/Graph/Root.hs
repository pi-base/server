{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graph.Root
  ( handler
  , asJSON
  , compiled
  , interpreted
  ) where

import Graph.Import
import Graph.Queries       as G (queries)
import Graph.Mutations     as G (mutations)

import           Data.Aeson          as Aeson
import qualified Graph.Queries.Cache as Cache
import           Graph.Schema        as G

handler :: (MonadGraph m, MonadLogger m) => AppSettings -> SchemaRoot m QueryRoot MutationRoot
handler settings = SchemaRoot G.queries (G.mutations $ appGithub settings)

asJSON :: MonadIO m
       => (QueryData -> m Response) 
       -> Aeson.Value 
       -> m Aeson.Value
asJSON executor request = case fromJSON request of
  Aeson.Error  err -> throwIO $ QuerySerializationError err
  Aeson.Success qd -> executor qd >>= \case
    (PreExecutionFailure errs) -> throwIO $ ExecutionErrors errs
    (ExecutionFailure    errs) -> throwIO $ ExecutionErrors errs
    (PartialSuccess _    errs) -> throwIO $ ExecutionErrors errs
    r -> return $ Aeson.toJSON r

interpreted :: forall m. (MonadGraph m, MonadLogger m) 
            => AppSettings -> QueryData -> m Response
interpreted settings QueryData{..} = interpretRequest @(G.Root m) (handler settings) query (unOp operation) (unVar variables)

compiled :: forall m. (MonadGraph m, MonadLogger m) 
         => AppSettings -> Cache.Cache -> QueryData -> m Response
compiled settings cache QueryData{..} = case unOp operation of
  Nothing -> throwIO QueryNameRequired
  Just name -> case Cache.query cache name of
    Nothing -> throwIO $ QueryNotFound name
    Just q  -> executeRequest @(G.Root m) (handler settings) q (Just name) (unVar variables)