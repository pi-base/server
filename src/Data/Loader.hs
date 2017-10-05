module Data.Loader
  ( Loader
  , mkLoader
  , cache
  , version
  ) where

import Core         hiding (Loader)
import Data.Git     (commitVersion)
import Types.Loader as Loader

instance Show Loader where
  show Loader{..} = "<Loader(" ++ show (commitVersion commit) ++ ")>"

mkField :: MonadBase IO m => Text -> m (Field a)
mkField name = Field
  <$> pure name
  <*> newIORef (error $ (show name) ++ " not loaded")
  <*> newMVar False

mkLoader :: MonadBase IO m => Commit LgRepo -> m Loader
mkLoader commit = Loader
  <$> pure commit
  <*> mkField "spaces"
  <*> mkField "properties"
  <*> mkField "theorems"
  <*> mkField "traits"

version :: Loader -> Version
version = commitVersion . Loader.commit

cache :: (MonadBase IO m, MonadBaseControl IO m, Monoid a, Eq a, Show a)
      => Field a -> m a -> m a
cache Field{..} action = modifyMVar loaded $ \ready -> do
  result <- if ready
    then do
      readIORef ref
    else do
      result <- action
      writeIORef ref result
      return result
  return (True, result)
