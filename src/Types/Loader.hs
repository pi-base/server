module Types.Loader where

import Import.NoFoundation hiding (Field)

import Data.Map    (Map)
import Git         (Commit)
import Git.Libgit2 (LgRepo)
import Types

data Field a = Field
  { name   :: Text
  , ref    :: IORef a
  , loaded :: MVar Bool
  }

data Loader = Loader
  { commit     :: Commit LgRepo
  , spaces     :: Field [Space]
  , properties :: Field [Property]
  , theorems   :: Field [Theorem PropertyId]
  , traits     :: Field (Map SpaceId (Map PropertyId TVal))
  }
