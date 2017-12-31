{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , TemplateHaskell
  , TypeApplications
  , TypeOperators
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.Root
  ( Root
  , handler
  , schema
  , asJSON
  , interpret
  , execute
  ) where

import Graph.Import

import Data.Aeson            as Aeson
import GraphQL.Value.ToValue (ToValue(..), toValue)

import           Graph.Mutations
import qualified Graph.Queries.Cache as Cache
import           Graph.Queries       as G
import           Graph.Types         as G

handler :: (MonadGraph m, MonadLogger m) => Handler m Root
handler = pure $ pure "Query"
  :<> G.viewer
  :<> G.user
  -- Mutations
  :<> createSpace
  :<> createProperty
  :<> updateSpace
  :<> updateProperty
  :<> updateTheorem
  :<> assertTrait 
  :<> assertTheorem
  :<> resetBranch

schema :: Either QueryError Schema
schema = makeSchema @Root

asJSON :: MonadThrow m 
     => (QueryData -> m Response) -> Aeson.Value -> m Aeson.Value
asJSON executor request = case fromJSON request of
  Aeson.Error  err -> throw $ QuerySerializationError err
  Aeson.Success qd -> (Aeson.toJSON . toValue) <$> executor qd

interpret :: (MonadGraph m, MonadLogger m) => QueryData -> m Response
interpret QueryData{..} = interpretQuery @Root handler query (unOp operation) (unVar variables)

execute :: (MonadGraph m, MonadLogger m) 
        => Cache.Cache -> QueryData -> m Response
execute cache QueryData{..} = case unOp operation of
  Nothing -> throw $ GraphError QueryNameRequired
  Just name -> Cache.query cache name >>= \case
    Nothing -> throw $ GraphError $ QueryNotFound name
    Just compiled -> executeQuery @Root handler compiled (Just name) (unVar variables)