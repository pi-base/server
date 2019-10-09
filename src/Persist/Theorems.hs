module Persist.Theorems
  ( Theorems
  , all
  , get
  , put
  , runState
  , runRepo
  , toState
  ) where

import Core

import           Data.Attoparsec.Text      (takeTill)
import qualified Data.Id                   as Id
import qualified Data.Theorem              as Theorem
import qualified Persist.Backend.Git       as Git
import qualified Persist.Backend.Git.Pages as Pages
import           Persist.Branches          (Branches)
import           Persist.Repo              (Repo)
import           Persist.Store             (Store, Action)
import qualified Persist.Store             as Store
import           Polysemy.State            (State, evalState)

type Theorems = Store Theorem TheoremId

all :: Member Theorems r => Sem r [Theorem]
all = Store.all

get :: Member Theorems r => TheoremId -> Sem r (Maybe Theorem)
get = Store.get

put :: Member Theorems r => Action -> Theorem -> Sem r ()
put c theorem = Store.put c (Theorem.id theorem) theorem

toState :: Sem (Theorems ': r) a
        -> Sem (State (Map TheoremId Theorem) ': r) a
toState = Store.toState CanonicalId

runState :: [Theorem]
         -> Sem (Theorems ': r) a
         -> Sem r a
runState theorems a = toState a
  & evalState (indexBy Theorem.id theorems)

runRepo :: Members '[Branches, Repo, Embed IO] r
       => Sem (Theorems ': r) a
       -> Sem r a
runRepo = Store.runRepo Pages.theorem
  $ Git.paths
    "theorems"
    (\id -> Id.encode id <> ".md")
    (takeTill (== '.') <* ".md" >>= Git.parseId)
