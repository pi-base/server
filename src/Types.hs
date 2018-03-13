{-# LANGUAGE
    DeriveFunctor
  , DeriveTraversable
  , FunctionalDependencies
  , Rank2Types
  , TemplateHaskell
#-}
module Types
  ( module Types
  , module X
  ) where

import Import.NoFoundation
import Control.Lens (Prism', makeLenses)

import Git (TreeFilePath)

import qualified Data.Aeson                  as Aeson (Object)
import qualified Data.HashMap.Strict         as HM
import qualified GraphQL                     as GraphQL (QueryError)
import qualified GraphQL.Internal.Output     as GraphQL (Errors)
import qualified GraphQL.Internal.Syntax.AST as GraphQL (Name)

import Types.Base as X

type    Uid     = Text
type    Record  = (TreeFilePath, Text)
newtype Version = Version { unVersion :: Text } deriving (Eq, ToJSON, FromJSON)

newtype Ref     = Ref Text deriving (Eq, Show)
type    Sha     = Text
data Committish = CommitRef Ref | CommitSha Sha deriving (Eq, Show)
type BranchName = Text

data CitationType = DOICitation | MRCitation | WikiCitation
  deriving (Show, Eq, Ord)

data Citation = Citation 
  { citationName :: Text
  , citationType :: CitationType
  , citationRef  :: Text
  } deriving (Show, Eq, Ord)

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

data GraphError = ExecutionErrors GraphQL.Errors
                | QueryNotFound GraphQL.Name
                | QueryNameRequired
                | QuerySerializationError String
                | SchemaInvalid GraphQL.QueryError
                deriving (Eq, Show)
                
data Conflict = Conflict { expectedSha :: Sha, actualSha :: Sha }
  deriving (Show, Eq)

data NotFoundError = NotFoundError
  { nfResource   :: Text
  , nfIdentifier :: Text
  } deriving (Show, Eq)

data PermissionError = BranchPermission BranchAccess
  deriving (Show, Eq)

data ValidationError = ValidationMessage Text -- TODO: specify structure
  deriving (Show, Eq)

data Error = ConflictError   Conflict
           | LogicError      LogicError
           | NotFound        NotFoundError
           | NotATree        TreeFilePath
           | ParseError      TreeFilePath String
           | PermissionError PermissionError
           | GraphError      GraphError
           | ValidationError ValidationError
           | UnknownGitRef   Ref
           deriving Eq

data Space = Space
  { spaceId          :: !SpaceId
  , spaceSlug        :: !Text
  , spaceName        :: !Text
  , spaceAliases     :: ![Text]
  , spaceDescription :: !Text
  , spaceTopology    :: !(Maybe Text)
  , spaceRefs        :: ![Citation]
  } deriving Eq

data Property = Property
  { propertyId          :: !PropertyId
  , propertySlug        :: !Text
  , propertyName        :: !Text
  , propertyAliases     :: ![Text]
  , propertyDescription :: !Text
  , propertyRefs        :: ![Citation]
  } deriving Eq

data Formula p = Atom p TVal
               | And [Formula p]
               | Or  [Formula p]
               deriving (Eq, Functor, Foldable, Traversable)

data Implication p = Implication (Formula p) (Formula p)
  deriving (Eq, Functor, Foldable, Traversable)

data Theorem p = Theorem
  { theoremId          :: !TheoremId
  , theoremImplication :: !(Implication p)
  , theoremConverse    :: !(Maybe [TheoremId])
  , theoremDescription :: !Text
  , theoremRefs        :: ![Citation]
  } deriving (Eq, Functor, Foldable, Traversable)

data Trait s p = Trait
  { _traitSpace       :: !s
  , _traitProperty    :: !p
  , _traitValue       :: !TVal
  , _traitRefs        :: ![Citation]
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
