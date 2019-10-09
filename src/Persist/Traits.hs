module Persist.Traits
  ( Traits
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
import qualified Data.Trait                as Trait
import qualified Persist.Backend.Git       as Git
import qualified Persist.Backend.Git.Pages as Pages
import           Persist.Branches          (Branches)
import           Persist.Repo              (Repo)
import           Persist.Store             (Store, Action)
import qualified Persist.Store             as Store
import           Polysemy.State            (State, evalState)

type Traits = Store Trait TraitId

all :: Member Traits r => Sem r [Trait]
all = Store.all

get :: Member Traits r => TraitId -> Sem r (Maybe Trait)
get = Store.get

put :: Member Traits r => Action -> Trait -> Sem r ()
put c trait = Store.put c (Trait.id trait) trait

toState :: Sem (Traits ': r) a
        -> Sem (State (Map TraitId Trait) ': r) a
toState = Store.toState (error "toState")

runState :: [Trait]
         -> Sem (Traits ': r) a
         -> Sem r a
runState traits a = toState a
  & evalState (indexBy Trait.id traits)

runRepo :: Members '[Branches, Repo, Embed IO] r
       => Sem (Traits ': r) a
       -> Sem r a
runRepo = Store.runRepo Pages.trait
  $ Git.paths
    "spaces"
    (\(space, property) -> Id.encode space <> "/properties/" <> Id.encode property <> ".md")
    (do
      space <- Git.parseId =<< takeTill (== '/')
      void "/properties/"
      property <- Git.parseId =<< takeTill (== '.')
      void ".md"
      return (space, property))
