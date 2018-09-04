{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
  ( module Types
  , module X
  ) where

import Import.NoFoundation
import Control.Lens (Prism', makeLenses)

import Git (TreeFilePath)

import qualified Data.Aeson              as Aeson (Object)
import qualified Data.HashMap.Strict     as HM
import qualified GraphQL                 as GraphQL (QueryError)
import qualified GraphQL.Internal.Name   as GraphQL (Name)
import qualified GraphQL.Internal.Output as GraphQL (Errors)

import Types.Base as X

{- GIT TYPES -}

type    BranchName = Text
data    Committish = CommitRef Ref | CommitSha Sha deriving (Eq, Show)
type    Record     = (TreeFilePath, Text)
newtype Ref        = Ref Text deriving (Eq, Show, IsString)
type    Sha        = Text
newtype Version    = Version { unVersion :: Text } deriving (Eq, Show, ToJSON, FromJSON)

data PageData = PageData
  { pagePath        :: !ByteString
  , pageFrontmatter :: !Aeson.Object
  , pageMain        :: !Text
  , pageSections    :: !(HM.HashMap Text Text)
  }

newtype Page a = Page (Prism' PageData a)

data CommitMeta = CommitMeta
  { commitUser    :: User
  , commitMessage :: Text
  } deriving Show

data BranchStatus = BranchStatus
  { branch       :: Branch
  , branchHead   :: Sha
  , branchAccess :: BranchAccess
  }

{- DATA MODEL -}

data CitationType = DOICitation | MRCitation | WikiCitation
  deriving (Show, Eq, Ord)

data Citation = Citation
  { citationName :: Text
  , citationType :: CitationType
  , citationRef  :: Text
  } deriving (Show, Eq, Ord)

type    Uid     = Text
newtype Id a = Id { unId :: Uid } deriving (Eq, Ord, Show, ToJSON, FromJSON)

type SpaceId    = Id Space
type PropertyId = Id Property
type TheoremId  = Id (Theorem PropertyId)

type TraitId = (SpaceId, PropertyId)
type TVal = Bool

data Space = Space
  { spaceId          :: !SpaceId
  , spaceCx          :: !(Maybe Int)
  , spaceName        :: !Text
  , spaceAliases     :: ![Text]
  , spaceDescription :: !Text
  , spaceTopology    :: !(Maybe Text)
  , spaceRefs        :: ![Citation]
  } deriving (Eq, Show)

data Property = Property
  { propertyId          :: !PropertyId
  , propertyCx          :: !(Maybe Int)
  , propertyName        :: !Text
  , propertyAliases     :: ![Text]
  , propertyDescription :: !Text
  , propertyRefs        :: ![Citation]
  } deriving (Eq, Show)

data Formula p = Atom p TVal
               | And [Formula p]
               | Or  [Formula p]
               deriving (Eq, Functor, Foldable, Show, Traversable)

data Implication p = Implication (Formula p) (Formula p)
  deriving (Eq, Functor, Foldable, Show, Traversable)

data Theorem p = Theorem
  { theoremId          :: !TheoremId
  , theoremImplication :: !(Implication p)
  , theoremConverse    :: !(Maybe [TheoremId])
  , theoremDescription :: !Text
  , theoremRefs        :: ![Citation]
  } deriving (Eq, Show, Functor, Foldable, Traversable)

data Trait s p = Trait
  { _traitSpace       :: !s
  , _traitProperty    :: !p
  , _traitValue       :: !TVal
  , _traitRefs        :: ![Citation]
  , _traitDescription :: !Text
  } deriving Show

makeLenses ''Trait

data Match = Yes | No | Unknown
  deriving (Show, Eq, Ord)

data Proof = Proof
  { proofSpace      :: SpaceId
  , proofProperties :: Set PropertyId
  , proofTheorems   :: Set TheoremId
  } deriving (Eq, Show)

data View = View
  { _viewProperties :: Map PropertyId Property
  , _viewSpaces     :: Map SpaceId    Space
  , _viewTheorems   :: Map TheoremId  (Theorem PropertyId)
  , _viewTraits     :: Map SpaceId    (Map PropertyId (Trait SpaceId PropertyId))
  , _viewProofs     :: Map TraitId    Proof
  , _viewVersion    :: Maybe Version
  } deriving Show

makeLenses ''View

type Properties = Map PropertyId TVal

{- EXCEPTIONS -}

data ConflictError = ConflictError { expectedSha :: Sha, actualSha :: Sha }
  deriving (Show, Typeable)

data ForcedError = ForcedError
  deriving (Show, Typeable)

data GraphError = ExecutionErrors GraphQL.Errors
                | QueryNotFound GraphQL.Name
                | QueryNameRequired
                | QuerySerializationError String
                | SchemaInvalid GraphQL.QueryError
                deriving (Show, Typeable)

data LoadError = LoadError TreeFilePath deriving (Show, Typeable)

data LogicError = Contradiction SpaceId PropertyId TVal TVal
                | Counterexamples [SpaceId]
                | LoadFailure LoadError
                deriving (Show, Typeable)

data NotFoundError = NotFoundError
  { nfResource   :: Text
  , nfIdentifier :: Text
  } deriving (Show, Typeable)

data ParseError = ParseError TreeFilePath Text
  deriving (Eq, Show, Typeable)

data PermissionError =
  BranchPermissionRequired
    { branch   :: Branch
    , required :: BranchAccess
    , actual   :: Maybe BranchAccess
    }
  deriving (Show, Typeable)

data ValidationError = ValidationMessage Text -- TODO: specify structure
  deriving (Show, Eq, Typeable)