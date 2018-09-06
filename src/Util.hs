module Util
  ( discardLeftC
  , groupBy
  , indexBy
  , insertNested
  , mapRightC
  , memoized
  , throwLeft
  , traverseDir
  , unionN
  ) where

import ClassyPrelude hiding (groupBy)

import           Conduit            hiding (throwM)
import qualified Data.Map           as M
import qualified Data.Set           as S
import           System.Directory   (listDirectory)
import           System.Posix.Files (getFileStatus, isDirectory)

groupBy :: Ord b => (a -> b) -> [a] -> M.Map b [a]
groupBy f = foldl' add M.empty
  where
    add m a = M.alter (app a) (f a) m

    app :: a -> Maybe [a] -> Maybe [a]
    app a Nothing   = Just [a]
    app a (Just as) = Just (a:as)

indexBy :: Ord b => (a -> b) -> [a] -> M.Map b a
indexBy f as = M.fromList $ map (\a -> (f a, a)) as

unionN :: Ord a => [S.Set a] -> S.Set a
unionN = foldl' S.union S.empty

insertNested :: (Ord a, Ord b) => a -> b -> v -> Map a (Map b v) -> Map a (Map b v)
insertNested a b v = M.alter add a
  where
    add = Just . M.insert b v . maybe mempty id

mapRightC :: Monad m => (t -> Either a b) -> ConduitM (Either a t) (Either a b) m ()
mapRightC f = awaitForever $ \ev -> yield $ ev >>= f

discardLeftC :: Monad m => ConduitM (Either a b) b m ()
discardLeftC = awaitForever $ either (const $ return ()) yield

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

memoized :: IORef (Maybe a) -> IO a -> IO a
memoized ref action = readIORef ref >>= \case
  Just val -> return val
  Nothing -> do
    val <- action
    writeIORef ref $ Just val
    return val

throwLeft :: (MonadIO m, Exception e) => Either e a -> m a
throwLeft (Left  e) = throwIO e
throwLeft (Right a) = return a