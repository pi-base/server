module Persist.Properties
  ( Properties
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
import qualified Data.Property             as Property
import qualified Persist.Backend.Git       as Git
import qualified Persist.Backend.Git.Pages as Pages
import           Persist.Branches          (Branches)
import           Persist.Repo              (Repo)
import           Persist.Store             (Store, Action)
import qualified Persist.Store             as Store
import           Polysemy.State            (State, evalState)

type Properties = Store Property PropertyId

all :: Member Properties r => Sem r [Property]
all = Store.all

get :: Member Properties r => PropertyId -> Sem r (Maybe Property)
get = Store.get

put :: Member Properties r => Action -> Property -> Sem r ()
put c property = Store.put c (Property.id property) property

toState :: Sem (Properties ': r) a
        -> Sem (State (Map PropertyId Property) ': r) a
toState = Store.toState CanonicalId

runState :: [Property]
         -> Sem (Properties ': r) a
         -> Sem r a
runState properties a = toState a
  & evalState (indexBy Property.id properties)

runRepo :: Members '[Branches, Repo, Embed IO] r
       => Sem (Properties ': r) a
       -> Sem r a
runRepo = Store.runRepo Pages.property
  $ Git.paths
    "properties"
    (\id -> Id.encode id <> ".md")
    (takeTill (== '.') <* ".md" >>= Git.parseId)
