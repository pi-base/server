module Util
  ( groupBy
  , indexBy
  , dupes
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
