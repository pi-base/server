{-# LANGUAGE TemplateHaskell #-}
module Data.Theorem
  ( fetch
  , put
  ) where

import Protolude hiding (find, put)

import           Core
import           Data          (required)
import qualified Data.Branch   as Branch
import qualified Data.Storable as Store

find :: (MonadStore m, MonadLogger m)
     => Branch -> TheoremId -> m (Maybe (Theorem Property))
find branch _id = Store.find branch _id >>= \case
  Nothing -> return Nothing
  Just theorem ->
    mapM (Store.find branch) theorem >>= return . sequence

fetch :: (MonadStore m, MonadLogger m)
      => Branch -> TheoremId -> m (Theorem Property)
fetch branch _id = find branch _id >>= Data.required "Theorem" (unId _id)

put :: (MonadStore m, MonadLogger m)
    => Branch
    -> User
    -> Text
    -> Theorem PropertyId
    -> m (Theorem Property, Sha)
put branch user message theorem = do
  -- TODO: verify deductions here?
  (_, sha) <- Branch.update branch user message $ \_ ->
    Store.write theorem
  -- TODO: don't go back to store?
  loaded <- fetch branch $ theoremId theorem
  return (loaded, sha)