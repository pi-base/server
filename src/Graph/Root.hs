{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , ExistentialQuantification
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
  , compiled
  , interpreted
  ) where

import Graph.Import

import qualified Control.Monad.Catch     as Catch
import           Data.Aeson              as Aeson

import qualified Core
import           Graph.Mutations
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
  :<> submitBranch settings

schema :: Either QueryError Schema
schema = makeSchema @Root

asJSON :: MonadCatch m 
     => (QueryData -> m Response) -> Aeson.Value -> m (Either Core.Error Aeson.Value)
asJSON executor request = case fromJSON request of
  Aeson.Error  err -> return . Left $ GraphError $ QuerySerializationError err
  Aeson.Success qd -> (trapErrors $ executor qd) >>= \case
    Left  e -> return $ Left e
    Right (PreExecutionFailure errs) -> return $ Left $ GraphError $ ExecutionErrors errs
    Right (ExecutionFailure    errs) -> return $ Left $ GraphError $ ExecutionErrors errs
    Right (PartialSuccess _    errs) -> return $ Left $ GraphError $ ExecutionErrors errs
    Right r -> return $ Right $ Aeson.toJSON r

trapErrors :: MonadCatch m => m a -> m (Either Core.Error a)
trapErrors action = catches (fmap Right action) 
  [ Catch.Handler (\e -> return $ Left (e :: Core.Error))
  ]

interpreted :: (MonadGraph m, MonadLogger m) => AppSettings -> QueryData -> m Response
interpreted settings QueryData{..} = interpretQuery @Root (handler settings) query (unOp operation) (unVar variables)

compiled :: (MonadGraph m, MonadLogger m) 
         => AppSettings -> Cache.Cache -> QueryData -> m Response
compiled settings cache QueryData{..} = case unOp operation of
  Nothing -> throw $ GraphError QueryNameRequired
  Just name -> Cache.query cache name >>= \case
    Nothing -> throw $ GraphError $ QueryNotFound name
    Just q  -> executeQuery @Root (handler settings) q (Just name) (unVar variables)