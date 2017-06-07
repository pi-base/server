module Util
  ( dupes
  , fetch
  , flatMapM
  , indexBy
  , groupBy
  , unionN
  ) where

import ClassyPrelude hiding (groupBy)

import qualified Data.Map as M
import qualified Data.Set as S

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
