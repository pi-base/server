{-# LANGUAGE DeriveFunctor #-}
module Types where

import Import.NoFoundation

import Git         (TreeFilePath)
import Git.Libgit2 (LgRepo)

import qualified Data.Set  as S
import qualified Data.Text as T

type    Uid     = Text
type    Record  = (TreeFilePath, Text)
newtype Version = Version { unVersion :: Text }

data Committish = Ref Text | Sha Text deriving (Eq, Show)

newtype SpaceId    = SpaceId { unSpaceId :: Uid } deriving (Eq, Ord, ToJSON, FromJSON)
newtype PropertyId = PropertyId { unPropertyId :: Uid } deriving (Eq, Ord, ToJSON, FromJSON)
newtype TheoremId  = TheoremId { unTheoremId :: Uid }   deriving (Eq, Ord, ToJSON, FromJSON)

type TraitId = (SpaceId, PropertyId)

data Error = NotATree TreeFilePath
           | ParseError TreeFilePath String
           | ReferenceError TreeFilePath [Uid]
           | NotUnique Text Text
           | CommitNotFound Committish
           deriving (Show, Eq)

data Space = Space
  { spaceId          :: !SpaceId
  , spaceSlug        :: !Text
  , spaceName        :: !Text
  , spaceDescription :: !Text
  , spaceTopology    :: !(Maybe Text)
  }

data Property = Property
  { propertyId          :: !PropertyId
  , propertySlug        :: !Text
  , propertyName        :: !Text
  , propertyAliases     :: !(Maybe [Text])
  , propertyDescription :: !Text
  }

data Formula p = Atom p Bool
               | And [Formula p]
               | Or  [Formula p]
               deriving (Eq, Functor)

data Implication p = Implication (Formula p) (Formula p)
  deriving (Eq, Functor)

data Theorem p = Theorem
  { theoremId          :: !TheoremId
  , theoremImplication :: !(Implication p)
  , theoremConverse    :: !(Maybe [TheoremId])
  , theoremDescription :: !Text
  } deriving (Eq, Functor)

data Trait s p = Trait
  { traitSpace       :: !s
  , traitProperty    :: !p
  , traitValue       :: !Bool
  , traitDescription :: !Text
  } deriving Show

data Match = Yes | No | Unknown
  deriving (Show, Eq, Ord)

data Assumptions = Assumptions
  { assumedTraits   :: S.Set TraitId
  , assumedTheorems :: S.Set TheoremId
  }

data Proof = Proof
  { proofFor      :: Trait Space Property
  , proofTheorems :: [Theorem Property]
  , proofTraits   :: [Trait Space Property]
  }

data Viewer = Viewer
  { viewerProperties :: [Property]
  , viewerSpaces     :: [Space]
  , viewerTheorems   :: [Theorem Property]
  , viewerTraits     :: [Trait Space Property]
  , viewerProofs     :: Map TraitId Assumptions
  , viewerVersion    :: Version
  }

-- TODO: enforce that only one thread gets to write to a branch at a time
data Store = Store
  { storeRepo  :: LgRepo
  , storeCache :: MVar (Maybe Viewer)
  }
