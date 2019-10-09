{-# LANGUAGE TemplateHaskell #-}
module Persist.Store
  ( Store(..)
  , Action(..)
  , State
  , all
  , get
  , initial
  , next
  , put
  , runRepo
  , toState
  ) where

import Core

import qualified Data.Map            as Map
import qualified Persist.Backend.Git as Git
import           Persist.Branches    (Branches)
import qualified Persist.Branches    as Branches
import qualified Persist.Repo        as Repo
import           Persist.Repo        (Repo, Action(..))
import qualified Polysemy.State      as S

data Store v k m a where
  All  :: Store v k m [v]
  Get  :: k -> Store v k m (Maybe v)
  Next :: Store v k m k
  Put  :: Action -> k -> v -> Store v k m ()

makeSem ''Store

type State = Map

initial :: Ord k => State k v
initial = mempty

toState :: Ord k
        => (Int -> k)
        -> Sem (Store v k ': r) a
        -> Sem (S.State (Map k v) ': r) a
toState toKey = reinterpret $ \case
  All       -> S.gets Map.elems
  Get     k -> S.gets $ Map.lookup k
  Next      -> toKey <$> S.gets Map.size
  Put _ k v -> S.modify $ Map.insert k v

runRepo ::
  ( Members '[Branches, Repo, Embed IO] r
  , Git.Pagable v
  )
  => Git.Page v k
  -> Git.Paths k
  -> Sem (Store (v Identity) k ': r) a
  -> Sem r a
runRepo pg pt = interpret $ \case
  All -> Repo.scan pg pt =<< Branches.current
  Get k -> do
    b <- Branches.current
    Repo.get pg pt b k
  Next    -> error "next" -- TODO: split this out to IdCursor and relax the Embed IO constraint
  Put a k v -> do
    b <- Branches.current
    Repo.put pg pt b a k v
