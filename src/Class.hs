{-# LANGUAGE AllowAmbiguousTypes #-}
module Class where

import ClassyPrelude

import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Logger        (MonadLogger)
import Control.Monad.Trans.Control (MonadBaseControl)
import Control.Monad.Trans.Reader  (ReaderT)
import Database.Persist.Sql
import Git                         (MonadGit)
import Git.Libgit2                 (LgRepo)

import Model
import Settings    (AppSettings(..))
import Types.Store

class MonadIO m => MonadDB m where
  db :: ReaderT SqlBackend m a -> m a

class (MonadBaseControl IO m, MonadIO m, MonadMask m, MonadGit LgRepo m) => MonadStore m where
  getStore :: m Store

class (MonadStore m, MonadDB m, MonadLogger m) => MonadGraph m where
  getSettings :: m AppSettings
  requireUser :: m (Entity User)
