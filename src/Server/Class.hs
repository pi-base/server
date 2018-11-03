{-# LANGUAGE MultiParamTypeClasses #-}
module Server.Class
  ( App
  , AppM
  , runApp
  ) where

import Core

import           Control.Lens         (view)
import           Control.Monad.Catch  (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Logger (MonadLogger(..))
import qualified Data.Text            as T
import           Database.Persist.Sql (runSqlPool)
import           Git.Libgit2          (HasLgRepo(..))
import qualified Network.Wreq         as Wreq
import           Servant              (ServantErr)
import           UnliftIO             (MonadUnliftIO)

import           Class            (Data, DB(..))
import           Data.Store.Types (storeRepo)
import           Http             (Http(..), jsonBody)
import qualified Logger

type App m = (DB m, Git m, HasEnv m, Http m, MonadError ServantErr m, MonadLogger m)

newtype AppM a = App
  { unApp :: ReaderT Env IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadUnliftIO, MonadCatch, MonadThrow, MonadMask, MonadReader Env)

runApp :: MonadIO m => Env -> AppM a -> m a
runApp env = liftIO . flip runReaderT env . unApp

-- TODO: consider https://github.com/morphismtech/squeal
instance DB AppM where
  db action = do
    pool <- view $ envFoundation . appConnPool
    runSqlPool action pool

instance Data AppM where
  getStore = view $ envFoundation . appStore

instance HasEnv AppM where
  getEnv = ask

instance HasLgRepo AppM where
  getRepository = view (envFoundation . appStore . storeRepo) <$> getEnv

instance Http AppM where
  get opts path = do
    res <- liftIO $ Wreq.getWith opts (T.unpack path)
    return $ jsonBody res
  post opts path body = do
    res <- liftIO $ Wreq.postWith opts (T.unpack path) body
    return $ jsonBody res

-- TODO: handle app-level errors in the app layer, not middleware
instance MonadError ServantErr AppM where
  catchError = catch
  throwError = throwIO

instance MonadLogger AppM where
  monadLoggerLog loc src lvl msg = do
    logger <- view $ envFoundation . appLogger
    Logger.logWithLoc logger loc src lvl msg
