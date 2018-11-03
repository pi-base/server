{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Class where

import Server.Import

import Test.Class.Quickcheck ()
import Test.Types

import Control.Lens          (view)
import Control.Monad.Logger  (MonadLogger(..), toLogStr)
import Database.Persist.Sql  (runSqlPool)
import System.Log.FastLogger (fromLogStr)

import Data.Store.Types (storeRepo)
import Http

instance DB TestM where
  db action = do
    pool <- view (envFoundation . appConnPool) <$> getEnv
    runSqlPool action pool

instance Data TestM where
  getStore = view (envFoundation . appStore) <$> getEnv

instance HasEnv TestM where
  getEnv = view foundation

instance HasLgRepo TestM where
  getRepository = view (envFoundation . appStore . storeRepo) <$> getEnv

instance Http TestM where
  get _opt path = do
    net <- view http
    res <- atomicModifyIORef' net $ \case
      (h:hs) -> (hs, Just h)
      _ -> ([], Nothing)
    case res of
      Just body -> return body
      Nothing -> throwIO $ NoRequestRegistered path
  post opt path _body = get opt path

instance MonadError ServantErr TestM where
  catchError = catch
  throwError = throwIO

instance MonadLogger TestM where
  monadLoggerLog _ _ _ msg = do
    l <- view log
    -- TODO: difference list and append?
    modifyIORef' l $ \ms -> (decodeUtf8 $ fromLogStr $ toLogStr msg) : ms
