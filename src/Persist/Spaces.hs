module Persist.Spaces
  ( Spaces
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
import qualified Data.Space                as Space
import qualified Persist.Backend.Git       as Git
import qualified Persist.Backend.Git.Pages as Pages
import           Persist.Branches          (Branches)
import           Persist.Repo              (Repo)
import           Persist.Store             (Store, Action)
import qualified Persist.Store             as Store
import           Polysemy.State            (State, evalState)

type Spaces = Store Space SpaceId

all :: Member Spaces r => Sem r [Space]
all = Store.all

get :: Member Spaces r => SpaceId -> Sem r (Maybe Space)
get = Store.get

put :: Member Spaces r => Action -> Space -> Sem r ()
put c space = Store.put c (Space.id space) space

toState :: Sem (Spaces ': r) a
        -> Sem (State (Map SpaceId Space) ': r) a
toState = Store.toState CanonicalId

runState :: [Space]
         -> Sem (Spaces ': r) a
         -> Sem r a
runState spaces a = toState a
  & evalState (indexBy Space.id spaces)

runRepo :: Members '[Branches, Repo, Embed IO] r
       => Sem (Spaces ': r) a
       -> Sem r a
runRepo = Store.runRepo Pages.space
  $ Git.paths
    "spaces"
    (\id -> Id.encode id <> "/README.md")
    (takeTill (== '/') <* "/README.md" >>= Git.parseId)
