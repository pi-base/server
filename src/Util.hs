module Util
  ( discardLeftC
  , dupes
  , encodeText
  , fetch
  , flatMapM
  , groupBy
  , indexBy
  , insertNested
  , mapRightC
  , traceC
  , unionN
  , traverseDir
  ) where

import ClassyPrelude hiding (groupBy)

import           Conduit            hiding (throwM)
import           Data.Aeson         (ToJSON, encode)
import qualified Data.Map           as M
import qualified Data.Set           as S
import qualified Data.Text.Lazy     as TL
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

dupes :: Ord a => [a] -> [a]
dupes = S.toList . snd . foldl' step (S.empty, S.empty)
  where
    step :: Ord a => (S.Set a, S.Set a) -> a -> (S.Set a, S.Set a)
    step (seen, dup) a = if S.member a seen
      then (seen, S.insert a dup)
      else (S.insert a seen, dup)

unionN :: Ord a => [S.Set a] -> S.Set a
unionN = foldl' S.union S.empty

flatMapM :: (Monoid (Element (t b)), MonoFoldable (t b), Traversable t, Monad m)
         => (a -> m b) -> t a -> m (Element (t b))
flatMapM f m = mapM f m >>= return . concat

data KeyError k = KeyError k deriving (Typeable, Show)

instance (Show k, Typeable k) => Exception (KeyError k)

fetch :: (MonadThrow m, Ord k, Show k, Typeable k) => k -> Map k v -> m v
fetch k m = case M.lookup k m of
  Just v  -> return v
  Nothing -> throwM $ KeyError k

encodeText :: ToJSON a => a -> Text
encodeText = TL.toStrict . decodeUtf8 . encode

insertNested :: (Ord a, Ord b) => a -> b -> v -> Map a (Map b v) -> Map a (Map b v)
insertNested a b v = M.alter add a
  where
    add = Just . M.insert b v . maybe mempty id

mapRightC :: Monad m => (t -> Either a b) -> ConduitM (Either a t) (Either a b) m ()
mapRightC f = awaitForever $ \ev -> yield $ ev >>= f

discardLeftC :: Monad m => ConduitM (Either a b) b m ()
discardLeftC = awaitForever $ either (const $ return ()) yield

traceC :: (Monad m, Show d) => (o -> d) -> ConduitM o o m ()
traceC f = mapC $ \x -> trace (show $ f x) x

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
