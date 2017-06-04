module Data.Parse
  ( viewer
  , spaces
  , properties
  , traits
  , theorems
  ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import           Data.Tagged           (Tagged(..))
import           Git

import Core
import Data.Git (commitVersion, eachBlob, lookupCommitish, useRepo)
import Util     (indexBy)

import qualified Page
import qualified Page.Property
import qualified Page.Space
import qualified Page.Theorem

viewer :: MonadStore m => Committish -> m (Either [Error] Viewer)
viewer commish = at commish $ \commit -> do
  (es, ss) <- spaces commit
  (ep, ps) <- properties commit
  (et, ts) <- theorems commit

  let (eh, ts') = hydrateTheorems ts ps

  let errors = es ++ ep ++ et ++ eh
  if length errors > 1
    then return $ Left errors
    else return $ Right $ Viewer ps ss ts' mempty mempty (commitVersion commit)

at :: MonadStore m
   => Committish
   -> (Commit LgRepo -> ReaderT LgRepo m (Either [Error] a))
   -> m (Either [Error] a)
at commish handler = useRepo $
  lookupCommitish commish >>= \case
    Nothing  -> return $ Left [CommitNotFound commish]
    Just ref -> (lookupCommit $ Tagged ref) >>= handler

spaces :: MonadStore m => Commit LgRepo -> ReaderT LgRepo m ([Error], [Space])
spaces commit = do
  tree  <- lookupTree $ commitTree commit
  blobs <- eachBlob tree "spaces"
  return $ parseEntries Page.Space.parser $ filter (\(path, _) -> isReadme path) blobs

properties :: MonadStore m => Commit LgRepo -> ReaderT LgRepo m ([Error], [Property])
properties commit = do
  tree  <- lookupTree $ commitTree commit
  blobs <- eachBlob tree "properties"
  return $ parseEntries Page.Property.parser blobs

theorems :: MonadStore m
         => Commit LgRepo -> ReaderT LgRepo m ([Error], [Theorem PropertyId])
theorems commit = do
  tree  <- lookupTree $ commitTree commit
  blobs <- eachBlob tree "theorems"
  return $ parseEntries Page.Theorem.parser blobs

traits :: MonadStore m
       => Commit LgRepo
       -> [Space]
       -> [Property]
       -> ReaderT LgRepo m ([Error], [Trait SpaceId PropertyId])
traits commit spaces properties = return ([], [])


parseEntries :: FromJSON f
             => Page.Parser f a
             -> [Record]
             -> ([Error], [a])
parseEntries parser = collectEithers . map parse
  where
    parse (path, contents) = Page.parse parser path contents

isReadme :: TreeFilePath -> Bool
isReadme path = "README.md" `BS.isSuffixOf` path

hydrateTheorems :: [Theorem PropertyId] -> [Property] -> ([Error], [Theorem Property])
hydrateTheorems ts ps =
  let index = indexBy propertyId ps
      lookup t = case hydrateTheorem index t of
        Left    ids -> Left . ReferenceError "hydrateTheorem" $ map unPropertyId ids
        Right found -> Right found
  in collectEithers $ map lookup ts

collectEithers :: [Either a b] -> ([a], [b])
collectEithers = foldl' acc ([], [])
  where
    acc :: ([a], [b]) -> Either a b -> ([a], [b])
    acc (as, bs) (Left  a) = (a : as, bs)
    acc (as, bs) (Right b) = (as, b : bs)
