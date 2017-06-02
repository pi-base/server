{-# LANGUAGE DeriveFunctor #-}
module Core
  ( module Core
  ) where

import ClassyPrelude                    as Core
import Control.Applicative              as Core ((<|>))
import Control.Monad.IO.Class           as Core (MonadIO, liftIO)
import Control.Monad.Reader             as Core (MonadReader, ReaderT,
                                                 ask, asks, runReaderT)
import Control.Monad.Trans              as Core (lift)
import Control.Monad.Trans.Control      as Core (MonadBaseControl)
import Control.Monad.Trans.State.Strict as Core (StateT)
import Data.ByteString                  as Core (ByteString)
import Data.Map                         as Core (Map)
import Data.Monoid                      as Core (Monoid)
import Data.Text                        as Core (Text)
import Git                              as Core (TreeFilePath)

import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=))
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Formula as F

type Uid = Text
type Record = (TreeFilePath, Text)
type Version = Text

newtype SpaceId = SpaceId { unSpaceId :: Uid } deriving (Eq, Ord, ToJSON, FromJSON)

instance Show SpaceId where
  show = T.unpack . unSpaceId

newtype PropertyId = PropertyId { unPropertyId :: Uid } deriving (Eq, Ord, ToJSON, FromJSON)

instance Show PropertyId where
  show = T.unpack . unPropertyId

newtype TheoremId  = TheoremId { unTheoremId :: Uid }   deriving (Eq, Ord, ToJSON, FromJSON)

instance Show TheoremId where
  show = T.unpack . unTheoremId

newtype TraitId = TraitId { unTraitId :: Uid } deriving (Eq, Ord, ToJSON, FromJSON)

instance Show TraitId where
  show = T.unpack . unTraitId

data Error = NotATree TreeFilePath
           | ParseError TreeFilePath String
           | ReferenceError TreeFilePath [Uid]
           | NotUnique Text Text
           deriving (Show, Eq)

explainError :: Error -> Text
explainError (NotATree path) = decodeUtf8 path <> ": could not find directory"
explainError (ParseError path msg) = decodeUtf8 path <> ": error while parsing - " <> T.pack msg
explainError (ReferenceError path ids) = decodeUtf8 path <> ": invalid reference - " <> (T.pack $ show ids)
explainError (NotUnique field value) = field <> " is not unique: " <> value

instance ToJSON Error where
  toJSON err = object
    [ "error" .= show err
    ]

data Space = Space
  { spaceId          :: !SpaceId
  , spaceSlug        :: !Text
  , spaceName        :: !Text
  , spaceDescription :: !Text
  , spaceTopology    :: !(Maybe Text)
  }

instance Show Space where
  show Space{..} = T.unpack $ "[" <> _id <> "|" <> spaceName <> "]"
    where
      SpaceId _id = spaceId

data Property = Property
  { propertyId          :: !PropertyId
  , propertySlug        :: !Text
  , propertyName        :: !Text
  , propertyAliases     :: !(Maybe [Text])
  , propertyDescription :: !Text
  }

instance Show Property where
  show Property{..} = T.unpack $ "[" <> _id <> "|" <> propertyName <> "]"
    where
      PropertyId _id = propertyId

data Implication p = Implication (F.Formula p) (F.Formula p)
  deriving (Eq, Functor)

implicationProperties :: Ord p => Implication p -> S.Set p
implicationProperties (Implication a c) = F.properties a `S.union` F.properties c

instance Show p => Show (Implication p) where
  show (Implication a c) = show a ++ " => " ++ show c

data Theorem p = Theorem
  { theoremId          :: !TheoremId
  , theoremIf          :: !(F.Formula p)
  , theoremThen        :: !(F.Formula p)
  , theoremConverse    :: !(Maybe [TheoremId])
  , theoremDescription :: !Text
  }

theoremImplication :: Theorem p -> Implication p
theoremImplication Theorem{..} = Implication theoremIf theoremThen

instance Show p => Show (Theorem p) where
  show t@Theorem{..} = "[" ++ show theoremId ++ "|" ++ show (theoremImplication t) ++ "]"

data Trait s p = Trait
  { traitId          :: !TraitId
  , traitSpace       :: !s
  , traitProperty    :: !p
  , traitValue       :: !Bool
  , traitDescription :: !Text
  -- , traitDeduced     :: !Bool
  }
  deriving Show

data Match = Yes | No | Unknown
  deriving (Show, Eq, Ord)

data Assumptions = Assumptions
  { assumedTraits   :: S.Set TraitId
  , assumedTheorems :: S.Set TheoremId
  } deriving Show

data Proof = Proof
  { proofFor      :: Trait Space Property
  , proofTheorems :: [Theorem Property]
  , proofTraits   :: [Trait Space Property]
  } deriving Show

(~>) :: F.Formula p -> F.Formula p -> Implication p
(~>) = Implication
infixl 3 ~>

converse :: Implication p -> Implication p
converse (Implication ant con) = Implication con ant

contrapositive :: Implication p -> Implication p
contrapositive (Implication ant con) = Implication (F.negate con) (F.negate ant)

negative :: Implication p -> Implication p
negative (Implication ant con) = Implication (F.negate ant) (F.negate con)

hydrateTheorem :: Ord a => Map a b -> Theorem a -> Either [a] (Theorem b)
hydrateTheorem props theorem =
  let
    (Implication a c) = theoremImplication theorem
  in
    case (F.hydrate props a, F.hydrate props c) of
      (Left as, Left bs) -> Left $ as ++ bs
      (Left as, _) -> Left as
      (_, Left bs) -> Left bs
      (Right a', Right c') -> Right $ theorem { theoremIf = a', theoremThen = c' }
