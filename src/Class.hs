{-# LANGUAGE 
    AllowAmbiguousTypes 
  , DeriveGeneric
  , StandaloneDeriving
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Class where

import ClassyPrelude

import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader  (ReaderT)
import Database.Persist.Sql        (Entity, SqlBackend)
import Git                         (MonadGit)
import Git.Libgit2                 (LgRepo)

import Data.Aeson (ToJSON(..), object, (.=))
import qualified Data.Map.Strict as SM
import qualified Data.Text       as T

import Formula     ()
import Model
import Settings     (AppSettings(..))
import Types
import Types.Store

class MonadIO m => MonadDB m where
  db :: ReaderT SqlBackend m a -> m a

class (MonadBaseControl IO m, MonadIO m, MonadMask m, MonadGit LgRepo m) => MonadStore m where
  getStore :: m Store

class (MonadStore m, MonadDB m) => MonadGraph m where
  getSettings :: m AppSettings
  requireUser :: m (Entity User)

instance Show (Id a) where
  show = T.unpack . unId

instance Show Space where
  show Space{..} = T.unpack $ "<" <> unId spaceId <> "|" <> spaceName <> ">"

instance Show Property where
  show Property{..} = T.unpack $ "<" <> unId propertyId <> "|" <> propertyName <> ">"

instance Show p => Show (Implication p) where
  show (Implication a c) = show a ++ " => " ++ show c

instance Show p => Show (Theorem p) where
  show Theorem{..} = "<" ++ show theoremId ++ "|" ++ show theoremImplication ++ ">"

instance Show Version where
  show = show . unVersion

deriving instance (Eq s, Eq p) => Eq (Trait s p)

deriving instance Show Proof
deriving instance Show View
deriving instance Show LogicError
deriving instance Show Error
deriving instance Show Branch

instance Exception Error
instance Exception [Error]
instance Exception GraphError

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

explainError :: Error -> Text
explainError (NotATree path) = decodeUtf8 path <> ": could not find directory"
explainError (ParseError path msg) = decodeUtf8 path <> ": error while parsing - " <> T.pack msg
explainError (ReferenceError path ids) = decodeUtf8 path <> ": invalid reference - " <> tshow ids
explainError (NotUnique field value) = field <> " is not unique: " <> value
explainError (CommitNotFound c) = "Could not find commit at " <> tshow c
-- FIXME: define these entirely
explainError e = tshow e

instance ToJSON Error where
  toJSON err = object
    [ "error" .= object
      [ "type"    .= show err
      , "message" .= explainError err
      ]
    ]