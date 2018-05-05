{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.Common 
  ( Config(..)
  , mkConfig
  , login
  , runGraph
  ) where

import TestImport (App(..), TestApp)

import           Control.Monad.Logger     (MonadLogger(..))
import           Database.Persist.Sql     (ConnectionPool, Entity(..), runSqlPool)
import           Git.Libgit2              (HasLgRepo(..))

import           Core
import qualified Data.Branch     as Branch
import           Data.Store      (storeRepo)
import           Handler.Helpers (ensureUser)
import           Settings        (AppSettings(..))

data Config = Config
  { pool     :: ConnectionPool
  , settings :: AppSettings
  , store    :: Store
  , user     :: Maybe (Entity User)
  }

instance MonadDB (ReaderT Config IO) where
  db action = asks pool >>= runSqlPool action
instance HasLgRepo (ReaderT Config IO) where
  getRepository = asks $ storeRepo . store
instance MonadStore (ReaderT Config IO) where
  getStore = asks store
instance MonadGraph (ReaderT Config IO) where
  requireUser = asks user >>= maybe (error "No user") return
  getSettings = asks settings
instance MonadLogger IO where
  monadLoggerLog _ _ _ _ = return () -- FIXME

mkConfig :: TestApp App -> Config
mkConfig (App{..}, _) = Config
  { pool     = appConnPool
  , settings = appSettings
  , store    = appStore
  , user     = Nothing
  }

login :: User -> Config -> IO Config
login user config = do
  currentUser <- runGraph config $ do
    entity <- ensureUser user
    _      <- Branch.ensureUserBranch entity
    return entity
  return $ config { user = Just currentUser }

runGraph :: Config -> ReaderT Config IO a -> IO a
runGraph conf action = runReaderT action conf