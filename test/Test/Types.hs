{-# LANGUAGE TemplateHaskell #-}
module Test.Types where

import Core

import Control.Monad.Catch (MonadThrow, MonadCatch, MonadMask)
import Data.Aeson          (Value)

data TestEnv = TestEnv
  { _foundation :: Env
  , _userRef    :: IORef (Maybe (Entity User))
  , _log        :: IORef [Text]
  , _http       :: IORef [Value]
  }

makeLenses ''TestEnv

newtype TestM a = Test
  { unTest :: ReaderT TestEnv IO a
  } deriving (Applicative, Functor, Monad, MonadIO, MonadUnliftIO, MonadCatch, MonadThrow, MonadMask, MonadReader TestEnv)

runTest :: MonadIO m => TestEnv -> TestM a -> m a
runTest e action = liftIO $ runReaderT (unTest action) e

type Runner = forall a. TestM a -> IO a

data NoRequestRegistered = NoRequestRegistered Text
  deriving (Show, Typeable)

instance Exception NoRequestRegistered
