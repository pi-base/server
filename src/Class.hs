{-# LANGUAGE AllowAmbiguousTypes, DeriveGeneric, StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Class where

import Protolude

import Control.Monad.Catch    (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Logger   (MonadLogger(..))
import Database.Persist.Sql   (Entity, SqlBackend)
import Git                    (MonadGit, TreeT)
import Git.Libgit2            (LgRepo)
import UnliftIO               (MonadUnliftIO)

import Data.Aeson (ToJSON(..), FromJSON(..), object, withObject, (.=), (.:))
import qualified Data.Map.Strict as SM

import Formula     ()
import Model
import Settings     (AppSettings(..), CISettings(..))
import Types
import Types.Store

class MonadIO m => MonadDB m where
  db :: ReaderT SqlBackend m a -> m a

class (MonadIO m, MonadUnliftIO m, MonadMask m, MonadGit LgRepo m) => MonadStore m where
  getStore :: m Store

class (MonadStore m, MonadDB m, MonadLogger m) => MonadGraph m where
  getSettings :: m AppSettings
  requireUser :: m (Entity User)

instance MonadLogger m => MonadLogger (TreeT r m) where
  monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

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

instance ToJSON CISettings where
  toJSON CISettings{..} = object [ "number" .= ciBuild, "sha" .= ciSha ]