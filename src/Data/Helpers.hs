module Data.Helpers
  ( findOrCreate
  , repsertBy
  ) where

import Import.NoFoundation
import Class

findOrCreate :: ( MonadDB m
                , PersistEntity record
                , PersistEntityBackend record ~ SqlBackend
                )
             => (record -> Unique record)
             -> record
             -> m (Entity record)
findOrCreate by obj = do
  mfound <- db . getBy $ by obj
  case mfound of
    Just entity -> return entity
    Nothing -> do
      _id <- db $ insert obj
      return $ Entity _id obj

repsertBy :: ( MonadDB m
             , PersistEntity record
             , PersistEntityBackend record ~ SqlBackend
             )
          => (record -> Unique record)
          -> record
          -> m (Entity record)
repsertBy by obj = db $ do
  mfound <- getBy $ by obj
  key <- case mfound of
    Just (Entity key _) -> do
      replace key obj
      return key
    _ -> insert obj
  return $ Entity key obj


