{-# LANGUAGE DataKinds , DeriveGeneric, ExistentialQuantification, TemplateHaskell, TypeApplications, TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.Root
  ( Root
  , handler
  , schema
  , asJSON
  , compiled
  , interpreted
  ) where

import Graph.Import
import Graph.Mutations

import           Data.Aeson          as Aeson
import qualified Graph.Queries.Cache as Cache
import           Graph.Queries       as G
import           Graph.Schema        as G

handler :: (MonadGraph m, MonadLogger m) => AppSettings -> Handler m Root
handler settings = pure $ pure "Query"
  :<> G.viewer
  :<> G.user
  -- Mutations
  :<> createSpace
  :<> createProperty
  :<> updateSpace
  :<> updateProperty
  :<> updateTheorem
  :<> updateTrait
  :<> assertTrait 
  :<> assertTheorem
  :<> resetBranch
  :<> submitBranch (appGithub settings)
  :<> approveBranch

schema :: Either QueryError Schema
schema = makeSchema @Root

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

interpreted :: (MonadGraph m, MonadLogger m) => AppSettings -> QueryData -> m Response
interpreted settings QueryData{..} = interpretQuery @Root (handler settings) query (unOp operation) (unVar variables)

compiled :: (MonadGraph m, MonadLogger m) 
         => AppSettings -> Cache.Cache -> QueryData -> m Response
compiled settings cache QueryData{..} = case unOp operation of
  Nothing -> throwIO QueryNameRequired
  Just name -> Cache.query cache name >>= \case
    Nothing -> throwIO $ QueryNotFound name
    Just q  -> executeQuery @Root (handler settings) q (Just name) (unVar variables)