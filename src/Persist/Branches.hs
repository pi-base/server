{-# LANGUAGE TemplateHaskell #-}
module Persist.Branches
  ( Branches
  , State
  , all
  , checkout
  , create
  , current
  , find
  , head
  , initial
  , master
  , runIO
  , runState
  , scan
  , submit
  , toState
  ) where

import           Core hiding (Version, empty, find, head)
import qualified Core (Version(..))

import           Control.Lens   (at, non, view, views)
import qualified Data.Branch    as Branch
import qualified Data.Map       as Map
import qualified Persist.DB     as DB
import qualified Persist.Github as Github
import qualified Persist.Repo   as Repo
import qualified Polysemy.State as S

data Branches m a where
  All      :: Branches m [Branch]
  Checkout :: Branch -> Branches m ()
  Create   :: Branch.Name -> Branches m Branch
  Current  :: Branches m Branch
  Find     :: Branch.Name -> Branches m (Maybe Branch)
  Master   :: Branches m Branch
  Submit   :: Branch -> Branches m (Either PullRequestError PullRequest)
  Version  :: Branch -> Branches m Core.Version

makeSem ''Branches

head :: Member Branches r => Sem r Core.Version
head = version =<< current

data State = State
  { _working  :: Maybe Branch
  , _branches :: Map Branch.Name Branch
  }

makeLenses ''State

initial :: State
initial = State Nothing mempty

toState :: Member Github.Github r
        => Branch
        -> Sem (Branches      ': r) a
        -> Sem (S.State State ': r) a
toState base = reinterpret \case
  Checkout b -> S.modify $ working .~ Just b
  Current    -> S.gets . view $ working . non base
  Master     -> return base

  All -> fmap Map.elems $ S.gets $ view branches
  Create n -> do
    let b = Branch.Branch n
    S.modify $ branches . at n .~ Just b
    return b
  Find name -> S.gets $ views branches $ Map.lookup name
  Version b -> return $ Core.Version $ Branch.name b

  Submit b -> Github.openPullRequest b base

runState :: Member Github.Github r
         => Branch
         -> Sem (Branches ': r) a
         -> Sem r a
runState base action = S.evalState initial $ toState base action

toIO :: Members '[Embed IO, DB.DB, Github.Github, Repo.Repo] r
     => Branch
     -> Sem (Branches ': r) a
     -> Sem (S.State Branch ': r) a
toIO base = reinterpret \case
  -- State Branch
  Checkout b -> S.modify $ const b
  Current    -> S.get
  Master     -> return base

  -- DB
  All -> DB.allBranches
  Create n -> do
    let b = Branch.Branch n
    Repo.createBranch b base
    void $ DB.ensureBranch b
    return b
  Find n -> DB.findBranch n

  -- Github
  Submit b -> Github.openPullRequest b base

  -- Repo
  Version b -> Repo.version b

runIO :: Members '[Embed IO, DB.DB, Github.Github, Repo.Repo] r
      => Branch
      -> Sem (Branches ': r) a
      -> Sem r a
runIO base action = toIO base action & S.evalState base

scan :: Members '[Branches, DB.DB, Repo.Repo] r => Sem r [Branch]
scan = do
  bs <- Repo.allBranches
  mapM_ DB.ensureBranch bs
  return bs
