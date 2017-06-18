{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Core
  ( module Core
  ) where

import ClassyPrelude                    as Core
import Control.Applicative              as Core ((<|>))
import Control.Monad.IO.Class           as Core (MonadIO, liftIO)
import Control.Monad.Reader             as Core (MonadReader(..), ReaderT, asks, runReaderT)
import Control.Monad.Trans              as Core (lift)
import Control.Monad.Trans.Control      as Core (MonadBaseControl)
import Control.Monad.Trans.Except       as Core (ExceptT, except, runExceptT, throwE)
import Control.Monad.Trans.State.Strict as Core (StateT, runStateT)
import Data.Aeson                       as Core (FromJSON, ToJSON)
import Data.ByteString                  as Core (ByteString)
import Data.Either                      as Core (partitionEithers)
import Data.Either.Combinators          as Core (mapLeft, mapRight)
import Data.Map                         as Core (Map)
import Data.Monoid                      as Core (Monoid)
import Data.Text                        as Core (Text)
import Data.Void                        as Core (Void)
import Git                              as Core (TreeFilePath, MonadGit, Commit, CommitMessage)
import Git.Libgit2                      as Core (LgRepo)

import Model as Core
import Types as Core

import Control.Lens hiding ((.=))
import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.Map.Strict as SM
import qualified Data.Set        as S
import qualified Data.Text       as T

import qualified Formula as F

explainError :: Error -> Text
explainError (NotATree path) = decodeUtf8 path <> ": could not find directory"
explainError (ParseError path msg) = decodeUtf8 path <> ": error while parsing - " <> T.pack msg
explainError (ReferenceError path ids) = decodeUtf8 path <> ": invalid reference - " <> tshow ids
explainError (NotUnique field value) = field <> " is not unique: " <> value
explainError (CommitNotFound c) = "Could not find commit at " <> tshow c
explainError e = tshow e

instance ToJSON Error where
  toJSON err = object
    [ "error" .= object
      [ "type"    .= show err
      , "message" .= explainError err
      ]
    ]

implicationProperties :: Ord p => Implication p -> S.Set p
implicationProperties (Implication a c) = F.properties a `S.union` F.properties c

theoremIf :: Theorem p -> Formula p
theoremIf t = let (Implication a _) = theoremImplication t in a

theoremThen :: Theorem p -> Formula p
theoremThen t = let (Implication _ c) = theoremImplication t in c

theoremProperties :: Ord p => Theorem p -> S.Set p
theoremProperties = implicationProperties . theoremImplication

theoremName :: Theorem Property -> Text
theoremName = T.pack . show . theoremImplication

traitId :: Trait Space Property -> TraitId
traitId = (,) <$> traitSpaceId <*> traitPropertyId

traitSpaceId :: Trait Space p -> SpaceId
traitSpaceId t = spaceId $ t ^. traitSpace

traitPropertyId :: Trait s Property -> PropertyId
traitPropertyId t = propertyId $ t ^. traitProperty

traitName :: Trait Space Property -> Text
traitName Trait{..} = spaceName _traitSpace <> ": " <> label <> propertyName _traitProperty
  where
    label = if _traitValue then "" else "~"

identifyTrait :: Trait Space Property -> Trait SpaceId PropertyId
identifyTrait = over traitSpace spaceId
              . over traitProperty propertyId

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
      (Right a', Right c') -> Right $ theorem { theoremImplication = Implication a' c' }

instance Show SpaceId where
  show = T.unpack . unSpaceId

instance Show PropertyId where
  show = T.unpack . unPropertyId

instance Show TheoremId where
  show = T.unpack . unTheoremId

instance Show Space where
  show Space{..} = T.unpack $ "[" <> unSpaceId spaceId <> "|" <> spaceName <> "]"

instance Show Property where
  show Property{..} = T.unpack $ "[" <> unPropertyId propertyId <> "|" <> propertyName <> "]"

instance Show p => Show (Implication p) where
  show (Implication a c) = show a ++ " => " ++ show c

instance Show p => Show (Theorem p) where
  show Theorem{..} = "[" ++ show theoremId ++ "|" ++ show theoremImplication ++ "]"

deriving instance Show Proof
deriving instance Show Version
deriving instance Show View

instance Exception Error
instance Exception [Error]

instance Monoid View where
  mappend a b = View
    { _viewProperties = mappend (_viewProperties a) (_viewProperties b)
    , _viewSpaces     = mappend (_viewSpaces a)     (_viewSpaces b)
    , _viewTheorems   = mappend (_viewTheorems a)   (_viewTheorems b)
    , _viewProofs     = mappend (_viewProofs a)     (_viewProofs b)
    , _viewTraits     = SM.unionWith mappend (_viewTraits a) (_viewTraits b)
    , _viewVersion    = Nothing
    }

  mempty = View mempty mempty mempty mempty mempty Nothing
