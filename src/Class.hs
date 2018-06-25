{-# LANGUAGE AllowAmbiguousTypes, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Class where

import Protolude

import Control.Monad.Catch    (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger   (MonadLogger)
import Database.Persist.Sql   (Entity, SqlBackend)
import Git                    (MonadGit)
import Git.Libgit2            (LgRepo)
import UnliftIO               (MonadUnliftIO)

import Data.Aeson (ToJSON(..), FromJSON(..), object, withObject, (.=), (.:))
import qualified Data.Map.Strict as SM

import Formula     ()
import Model
import Settings     (AppSettings(..))
import Types
import Types.Store

class MonadIO m => MonadDB m where
  db :: ReaderT SqlBackend m a -> m a

class (MonadIO m, MonadUnliftIO m, MonadMask m, MonadGit LgRepo m) => MonadStore m where
  getStore :: m Store

class (MonadStore m, MonadDB m, MonadLogger m) => MonadGraph m where
  getSettings :: m AppSettings
  requireUser :: m (Entity User)

{-
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
-}

deriving instance (Eq s, Eq p) => Eq (Trait s p)
deriving instance Generic CitationType

instance Exception ConflictError
instance Exception ForcedError
instance Exception GraphError
instance Exception LoadError
instance Exception LogicError
instance Exception NotFoundError
instance Exception ParseError
instance Exception PermissionError
instance Exception ValidationError

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

instance ToJSON Citation where
  toJSON Citation{..} =
    let type' = case citationType of
          DOICitation  -> "doi"
          MRCitation   -> "mr"
          WikiCitation -> "wikipedia"
    in object [ type' .= citationRef, ("name" :: Text) .= citationName ]

instance FromJSON Citation where
  parseJSON = withObject "Citation" $ \c -> do
    citationName <- c .: "name"
    (citationType, citationRef) <- getRef c "doi" DOICitation
                               <|> getRef c "mr" MRCitation
                               <|> getRef c "wikipedia" WikiCitation
    return Citation{..}
    where
      getRef c text type' = do
        citationRef <- c .: text
        return $ (type', citationRef)
    