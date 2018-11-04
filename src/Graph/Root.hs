{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Graph.Root
  ( opName
  , run
  ) where

import Graph.Import

import qualified Core
import           Http
import           Graph.Queries       as G (queries)
import           Graph.Mutations     as G (mutations)
import           Graph.Queries.Cache as Cache
import           Graph.Schema        as G
import           Graph.Types         (Context(..), unName)

handler :: Graph m => SchemaRoot m QueryRoot MutationRoot
handler = SchemaRoot G.queries G.mutations

interpreted :: forall m. Graph m => QueryData -> m Response
interpreted QueryData{..} =
  interpretRequest @(G.Root m) handler query (unOp operation) (unVar variables)

compiled :: forall m. Graph m => QueryCache -> QueryData -> m Response
compiled cache QueryData{..} = case unOp operation of
  Nothing -> throwIO QueryNameRequired
  Just name -> case Cache.fetch cache name of
    Nothing -> throwIO $ QueryNotFound name
    Just q  -> executeRequest @(G.Root m) handler q (Just name) (unVar variables)

run :: (DB m, HasEnv m, Git m, Http m, MonadLogger m)
    => Env
    -> Maybe (Entity Core.User)
    -> QueryData
    -> m Response
run env user req = do
  let context = Context
        { _currentUser    = user
        , _githubSettings = env ^. envSettings . Core.githubSettings
        , _testMode       = env ^. envSettings . Core.testMode
        }
      exec = case env ^. envFoundation . appQueries of
        Just cache -> compiled cache
        Nothing    -> interpreted
  runReaderT (exec req) context

opName :: Operation -> Maybe Text
opName op = unName <$> unOp op
