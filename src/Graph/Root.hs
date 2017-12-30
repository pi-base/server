{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , TemplateHaskell
  , TypeApplications
  , TypeOperators
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.Root
  ( asJSON
  , interpret
  , execute
  ) where

import Graph.Import

import Data.Aeson                  as Aeson
import GraphQL.Value.ToValue       (ToValue(..), toValue)
import GraphQL.Internal.Validation (QueryDocument, VariableValue)

import Core (Error(..))

import Graph.Mutations
import Graph.Queries   as G
import Graph.Types     as G

root :: (MonadGraph m, MonadLogger m) => Handler m Root
root = pure $ pure "Query"
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

asJSON :: MonadThrow m 
     => (QueryData -> m Response) -> Aeson.Value -> m Aeson.Value
asJSON handler request = case fromJSON request of
  Aeson.Error  err -> throw $ QueryError $ pack err
  Aeson.Success qd -> (Aeson.toJSON . toValue) <$> handler qd

interpret :: (MonadGraph m, MonadLogger m) => QueryData -> m Response
interpret QueryData{..} = interpretQuery @Root root query (unOp operation) (unVar variables)

-- TODO: maintain a name => compiled query document map and fetch by name
execute :: (MonadGraph m, MonadLogger m) 
        => QueryDocument VariableValue -> QueryData -> m Response
execute compiled QueryData{..} = case unOp operation of
  Just name -> executeQuery @Root root compiled (Just name) (unVar variables)
  Nothing   -> throw $ GeneralError "query name required"