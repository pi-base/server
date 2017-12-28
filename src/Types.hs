{-# LANGUAGE
    DeriveFunctor
  , DeriveTraversable
  , FunctionalDependencies
  , Rank2Types
#-}
module Types
  ( module Types
  , module X
  ) where

import Import.NoFoundation
import Control.Lens (Prism', makeLenses)

import Git (TreeFilePath)

import qualified Data.Aeson          as Aeson (Object)
import qualified Data.HashMap.Strict as HM

import Types.Base as X

type    Uid     = Text
type    Record  = (TreeFilePath, Text)
newtype Version = Version { unVersion :: Text } deriving (Eq, ToJSON, FromJSON)

newtype Ref     = Ref Text deriving (Eq, Show)
type    Sha     = Text
data Committish = CommitRef Ref | CommitSha Sha deriving (Eq, Show)

newtype Id a = Id { unId :: Uid } deriving (Eq, Ord, ToJSON, FromJSON)

type SpaceId    = Id Space
type PropertyId = Id Property
type TheoremId  = Id (Theorem PropertyId)

type TraitId = (SpaceId, PropertyId)
type TVal = Bool

data LoadError = LoadError TreeFilePath deriving (Eq, Show)

data LogicError = Contradiction SpaceId PropertyId TVal TVal
                | Counterexamples [SpaceId]
                | LoadFailure Error
                deriving Eq

-- TODO: make sure error handling is consistent throughout the application
--       and never stringly-typed
data Error = CommitNotFound Committish
           | LogicError     LogicError
           | NotFound       Text
           | NotATree       TreeFilePath
           | NotUnique      Text Text
           | ParseError     TreeFilePath String
           | PersistError   String
           | ReferenceError TreeFilePath [Uid]
           | UnknownGitRef  Ref
           | GeneralError   Text
           deriving Eq

data Space = Space
  { spaceId          :: !SpaceId
  , spaceSlug        :: !Text
  , spaceName        :: !Text
  , spaceAliases     :: ![Text]
  , spaceDescription :: !Text
  , spaceTopology    :: !(Maybe Text)
  } deriving Eq

data Property = Property
  { propertyId          :: !PropertyId
  , propertySlug        :: !Text
  , propertyName        :: !Text
  , propertyAliases     :: ![Text]
  , propertyDescription :: !Text
  } deriving Eq

data Formula p = Atom p TVal
               | And [Formula p]
               | Or  [Formula p]
               deriving (Eq, Functor, Foldable, Traversable)

data Implication p = Implication (Formula p) (Formula p)
  deriving (Eq, Functor)

data Theorem p = Theorem
  { theoremId          :: !TheoremId
  , theoremImplication :: !(Implication p)
  , theoremConverse    :: !(Maybe [TheoremId])
  , theoremDescription :: !Text
  } deriving (Eq, Functor)

data Trait s p = Trait
  { _traitSpace       :: !s
  , _traitProperty    :: !p
  , _traitValue       :: !TVal
  , _traitDescription :: !Text
  } deriving Show

makeLenses ''Trait

data PageData = PageData
  { pagePath        :: !ByteString
  , pageFrontmatter :: !Aeson.Object
  , pageMain        :: !Text
  , pageSections    :: !(HM.HashMap Text Text)
  }

newtype Page a = Page (Prism' PageData a)

data Match = Yes | No | Unknown
  deriving (Show, Eq, Ord)

data Proof = Proof
  { proofSpace      :: SpaceId
  , proofProperties :: Set PropertyId
  , proofTheorems   :: Set TheoremId
  }

data View = View
  { _viewProperties :: Map PropertyId Property
  , _viewSpaces     :: Map SpaceId    Space
  , _viewTheorems   :: Map TheoremId  (Theorem PropertyId)
  , _viewTraits     :: Map SpaceId    (Map PropertyId (Trait SpaceId PropertyId))
  , _viewProofs     :: Map TraitId    Proof
  , _viewVersion    :: Maybe Version
  }

makeLenses ''View

type Properties = Map PropertyId TVal

data CommitMeta = CommitMeta
  { commitUser    :: User
  , commitMessage :: Text
  }

data BranchStatus = BranchStatus
  { branch       :: Branch
  , branchHead   :: Sha
  , branchAccess :: BranchAccess
  }
