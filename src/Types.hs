{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
module Types where

import Import.NoFoundation
import Control.Lens (makeLenses)

import Git         (TreeFilePath)
import Git.Libgit2 (LgRepo)

import qualified Data.Map.Strict as M
import qualified Data.Set        as S

type    Uid     = Text
type    Record  = (TreeFilePath, Text)
newtype Version = Version { unVersion :: Text } deriving (Eq, ToJSON, FromJSON)

data Committish = Ref Text | Sha Text deriving (Eq, Show)

newtype SpaceId    = SpaceId    { unSpaceId    :: Uid } deriving (Eq, Ord, ToJSON, FromJSON)
newtype PropertyId = PropertyId { unPropertyId :: Uid } deriving (Eq, Ord, ToJSON, FromJSON)
newtype TheoremId  = TheoremId  { unTheoremId  :: Uid } deriving (Eq, Ord, ToJSON, FromJSON)

type TraitId = (SpaceId, PropertyId)

data LogicError = AssertionError deriving (Show, Eq)

data Error = NotATree TreeFilePath
           | ParseError TreeFilePath String
           | ReferenceError TreeFilePath [Uid]
           | NotUnique Text Text
           | CommitNotFound Committish
           | NotFound Text
           | LogicError LogicError
           | PersistError String
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
  { traitSpace       :: !s
  , traitProperty    :: !p
  , traitValue       :: !Bool
  , traitDescription :: !Text
  } deriving Show

data Match = Yes | No | Unknown
  deriving (Show, Eq, Ord)

data Proof = Proof
  { proofSpace      :: SpaceId
  , proofProperties :: S.Set PropertyId
  , proofTheorems   :: S.Set TheoremId
  }

data View = View
  { _viewProperties :: M.Map PropertyId Property
  , _viewSpaces     :: M.Map SpaceId    Space
  , _viewTheorems   :: M.Map TheoremId  (Theorem PropertyId)
  , _viewTraits     :: M.Map SpaceId    (M.Map PropertyId (Trait SpaceId PropertyId))
  , _viewProofs     :: M.Map TraitId    Proof
  , _viewVersion    :: Maybe Version
  }

makeLenses ''View

-- TODO: enforce that only one thread gets to write to a branch at a time
data Store = Store
  { storeRepo  :: LgRepo
  , storeCache :: MVar (Maybe View)
  }

class (MonadBaseControl IO m, MonadIO m, MonadMask m) => MonadStore m where
  getStore :: m Store

instance (MonadStore m) => MonadStore (ReaderT LgRepo m) where
  getStore = lift getStore
