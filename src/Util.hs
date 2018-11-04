module Util
  ( encodeText
  , fetch
  , findOrCreate
  , groupBy
  , indexBy
  , insertNested
  , repsertBy
  , slugify
  , traverseDir
  ) where

import Core hiding (replace)

import           Data.Aeson               (ToJSON, encode)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Char                (isAlpha, toLower)
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           Database.Persist
import           Database.Persist.Sql
import           System.Directory         (listDirectory)
import           System.Posix.Files       (getFileStatus, isDirectory)
import           System.FilePath.Posix    ((</>))

groupBy :: Ord b => (a -> b) -> [a] -> M.Map b [a]
groupBy f = foldl' add M.empty
  where
    add m a = M.alter (app a) (f a) m

    app :: a -> Maybe [a] -> Maybe [a]
    app a Nothing   = Just [a]
    app a (Just as) = Just (a:as)

indexBy :: Ord b => (a -> b) -> [a] -> M.Map b a
indexBy f as = M.fromList $ map (\a -> (f a, a)) as

data KeyError k = KeyError k deriving (Typeable, Show)

instance (Show k, Typeable k) => Exception (KeyError k)

fetch :: (MonadIO m, Ord k, Show k, Typeable k) => k -> Map k v -> m v
fetch k m = case M.lookup k m of
  Just v  -> return v
  Nothing -> throwIO $ KeyError k

encodeText :: ToJSON a => a -> Text
encodeText = decodeUtf8 . LBS.toStrict . encode

insertNested :: (Ord a, Ord b) => a -> b -> v -> Map a (Map b v) -> Map a (Map b v)
insertNested a b v = M.alter add a
  where
    add = Just . M.insert b v . maybe mempty identity

slugify :: Text -> Text
slugify = T.foldl convert ""
  where
    convert acc c
      | isAlpha c = acc `T.snoc` toLower c
      | otherwise = acc `T.snoc` '-'

traverseDir :: (a -> FilePath -> IO a) -> FilePath -> a -> IO a
traverseDir f top = go [top]
  where
    go (path : rest) acc = do
      stat <- getFileStatus path
      if isDirectory stat
        then do
          children <- listDirectory path
          go (rest <> map (\d -> path </> d) children) acc
        else f acc path >>= go rest
    go [] acc = return acc

findOrCreate :: ( DB m
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

repsertBy :: ( DB m
             , PersistEntity record
             , PersistEntityBackend record ~ SqlBackend
             )
          => (record -> Unique record)
          -> record
          -> m (Entity record)
repsertBy by obj = do
  mfound <- db $ getBy $ by obj
  key <- case mfound of
    Just (Entity key _) -> do
      db $ replace key obj
      return key
    _ -> db $ insert obj
  return $ Entity key obj
