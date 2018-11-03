{-# OPTIONS_GHC -fno-warn-orphans #-}
module Class where

import Protolude

import           Control.Monad.Catch    (MonadThrow, MonadCatch, MonadMask)
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Logger   (MonadLogger(..))
import           Control.Monad.Trans    (MonadTrans(..))
import           Control.Lens           (view)
import           Data.Aeson             (ToJSON(..), FromJSON(..), object, withObject, (.=), (.:))
import           Database.Persist.Sql   (Entity, SqlBackend)
import           Git                    (TreeT)
import           Git.Libgit2            (HasLgRepo(..))
import           UnliftIO               (MonadUnliftIO)

import           Data.Store.Types (Store(..))
import           Formula          ()
import qualified Graph.Types      as Graph
import           Http
import           Model
import           Settings
import           Types

-- TODO: why does
--   instance Data m => HasLgRepo m
-- lead to overlapping instances?
type Git m = (Data m, HasLgRepo m)
type MonadExcept m = (MonadThrow m, MonadCatch m, MonadMask m)

class DB m => Auth m where
  currentUser :: m (Maybe (Entity User))

class (MonadExcept m, MonadUnliftIO m) => Data m where
  getStore :: m Store

class MonadIO m => DB m where
  db :: ReaderT SqlBackend m a -> m a

class Monad m => HasEnv m where
  getEnv :: m Env

class (Auth m, Git m, Http m, MonadLogger m) => Graph m where
  getContext :: m Graph.Context

instance DB m => Auth (ReaderT Graph.Context m) where
  currentUser = view Graph.currentUser

instance Data m => Data (ReaderT a m) where
  getStore = lift getStore

instance DB m => DB (ReaderT a m) where
  db sql = lift (db ask) >>= runReaderT sql

instance (Monad m, HasLgRepo m) => HasLgRepo (ReaderT Graph.Context m) where
  getRepository = lift getRepository

instance Http m => Http (ReaderT Graph.Context m) where
  get opts path = lift $ Http.get opts path
  post opts path body = lift $ Http.post opts path body

instance (DB m, Git m, Http m, MonadLogger m) => Graph (ReaderT Graph.Context m) where
  getContext = ask

instance MonadLogger m => MonadLogger (TreeT r m) where
  monadLoggerLog a b c d = lift $ monadLoggerLog a b c d

deriving instance (Eq s, Eq p) => Eq (Trait s p)
deriving instance Generic CitationType

instance Exception NotAuthenticated
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
