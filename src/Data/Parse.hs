module Data.Parse
  ( viewer
  , viewSpace
  , at
  , spaceEntries
  , spaces
  , propertyEntries
  , properties
  , traitEntries
  , traits
  , theoremEntries
  , theorems
  , findProperty
  , findSpace
  , sinkMap
  , parseEntry
  , blobs
  ) where

import           Control.Monad.State.Strict (modify)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.Tagged           (Tagged(..))
import           Git

import Core
import Conduit
import Control.Lens (set)
import Data.Git     (commitVersion, getDir, lookupCommittish, useRepo)
import Util         (indexBy)

import qualified Page
import qualified Page.Property
import qualified Page.Space
import qualified Page.Theorem
import qualified Page.Trait
import qualified View as V

type ET m = ExceptT Error (StateT [Error] m)

fatal :: Monad m => Error -> ET m a
fatal = throwE

warn :: Monad m => Error -> ET m ()
warn err = modify (err :)

withErrors :: Monad m => ET m a -> m (Either [Error] a)
withErrors action = runStateT (runExceptT action) [] >>= \case
  (Right val, errs) -> if length errs == 0
    then return $ Right val
    else return $ Left errs
  (Left err, errs) -> return . Left $ err : errs

noteErrors :: Monad m => m [Either Error a] -> ET m [a]
noteErrors m = do
  (errs, vals) <- partitionEithers <$> (lift $ lift m)
  mapM_ warn errs
  return vals

required :: Monad m => Either Error a -> ET m a
required (Right val) = return val
required (Left  err) = fatal err

unique :: (a -> b) -> [a] -> Either Error ()
unique f as = return () -- FIXME

findProperty :: MonadStore m => Committish -> PropertyId -> m (Maybe Property)
findProperty commish pid = at commish $ \commit ->
  runConduit $ properties commit .| sinkFind (\p -> propertyId p == pid)

findSpace :: MonadStore m => Committish -> SpaceId -> m (Maybe Space)
findSpace commish sid = at commish $ \commit ->
  runConduit $ spaces commit .| sinkFind (\s -> spaceId s == sid)

viewSpace :: MonadStore m => SpaceId -> Committish -> m (Either [Error] View)
viewSpace sid commish = at commish $ \commit -> withErrors $ do
  ss <- noteErrors $ runConduit $ spaces commit .| sinkList
  s  <- required $ findSpace ss

  ps <- noteErrors $ runConduit $ properties commit .| sinkList
  required $ unique propertyId ps

  is <- noteErrors $ runConduit $ theorems commit ps .| sinkList
  required $ unique theoremId is

  ts <- noteErrors $ runConduit $ traits commit [s] ps .| sinkList
  required $ unique traitId ts

  return $ V.build [s] ps ts is (commitVersion commit)

  where
    findSpace :: [Space] -> Either Error Space
    findSpace ss = case find (\s -> spaceId s == sid) ss of
      Nothing -> Left . NotFound $ "Space " <> tshow sid
      Just s  -> Right s

viewer :: MonadStore m => Committish -> m (Either [Error] View)
viewer commish = at commish $ \commit -> withErrors $ do
  ss <- noteErrors $ runConduit $ spaces commit .| sinkList
  required $ unique spaceId ss

  ps <- noteErrors $ runConduit $ properties commit .| sinkList
  required $ unique propertyId ps

  is <- noteErrors $ runConduit $ theorems commit ps .| sinkList
  required $ unique theoremId is

  ts <- noteErrors $ runConduit $ traits commit ss ps .| sinkList
  required $ unique traitId ts

  return $ V.build ss ps ts is (commitVersion commit)

at :: (MonadStore m, MonadThrow m)
   => Committish
   -> (Commit LgRepo -> ReaderT LgRepo m a)
   -> m a
at commish handler = useRepo $
  lookupCommittish commish >>= \case
    Nothing  -> Core.throwM [CommitNotFound commish]
    Just ref -> lookupCommit (Tagged ref) >>= handler

type EntrySource m r = ConduitM () (TreeFilePath, TreeEntry r) m ()

spaceEntries :: MonadGit r m => Commit r -> EntrySource m r
spaceEntries commit = sourceCommitEntries commit "spaces"
                   .| filterC (isReadme . fst)

spaces :: MonadStore m
       => Commit LgRepo
       -> ConduitM () (Either Error Space) (ReaderT LgRepo m) ()
spaces commit = spaceEntries commit .| parseEntry Page.Space.page


propertyEntries :: MonadGit r m => Commit r -> EntrySource m r
propertyEntries commit = sourceCommitEntries commit "properties"

properties :: MonadStore m
           => Commit LgRepo
           -> ConduitM () (Either Error Property) (ReaderT LgRepo m) ()
properties commit = propertyEntries commit .| parseEntry Page.Property.page

theoremEntries :: MonadGit r m => Commit r -> EntrySource m r
theoremEntries commit = sourceCommitEntries commit "theorems"

theorems :: MonadStore m
         => Commit LgRepo
         -> [Property]
         -> ConduitM () (Either Error (Theorem Property)) (ReaderT LgRepo m) ()
theorems commit ps = theoremEntries commit
                  .| parseEntry Page.Theorem.page
                  .| mapRightC hydrate
  where
    hydrate = mapLeft (ReferenceError "hydrateTheorem" . map unPropertyId)
            . hydrateTheorem (indexBy propertyId ps)

traitEntries :: MonadGit r m => Commit r -> EntrySource m r
traitEntries commit = sourceCommitEntries commit "spaces"
                   .| filterC (not . isReadme . fst)

traits :: MonadStore m
       => Commit LgRepo
       -> [Space]
       -> [Property]
       -> ConduitM () (Either Error (Trait Space Property)) (ReaderT LgRepo m) ()
traits commit ss ps = traitEntries commit
                   .| filterC (\(path, _) -> relevant path)
                   .| parseEntry Page.Trait.page
                   -- .| mapC (fmap fst)
                   .| mapRightC (hydrateTrait sx px)
  where
    sx :: M.Map SpaceId Space
    sx = indexBy spaceId ss

    px :: M.Map PropertyId Property
    px = indexBy propertyId ps

    relevant :: TreeFilePath -> Bool
    relevant path = S.member path paths

    -- TODO: split and then check rather than building this whole n*n size set
    paths :: S.Set TreeFilePath
    paths = S.fromList [ (encodeUtf8 $ spaceSlug s) <> "/properties/" <> (encodeUtf8 $ propertySlug p) <> ".md" | s <- ss, p <- ps ]

isReadme :: TreeFilePath -> Bool
isReadme path = "README.md" `BS.isSuffixOf` path

sourceCommitEntries :: MonadGit r m
                    => Commit r
                    -> TreeFilePath
                    -> ConduitM i (TreeFilePath, TreeEntry r) m ()
sourceCommitEntries commit path = do
  edir <- lift $ do
    tree <- lookupTree $ commitTree commit
    getDir tree path
  case edir of
    Left    _ -> return ()
    Right dir -> sourceTreeEntries dir .| mapC (\(p,t) -> (path <> "/" <> p, t))

blobs :: MonadGit r m => ConduitM (TreeFilePath, TreeEntry r) (TreeFilePath, Text) m ()
blobs = awaitForever $ \(path, entry) -> case entry of
  (BlobEntry _id _) -> do
    blob <- lift $ catBlobUtf8 _id
    yield (path, blob)
  _ -> return ()

parseEntry :: (MonadStore m, MonadGit r m)
           => Page a
           -> ConduitM
             (TreeFilePath, TreeEntry r)
             (Either Error a)
             m ()
parseEntry page = blobs .| mapC (\(path, blob) -> Page.parse page (path, blob))

-- TODO: clean this up now that trait is a lens
hydrateTrait :: Map SpaceId s -> Map PropertyId p -> Trait SpaceId PropertyId -> Either Error (Trait s p)
hydrateTrait sx px t@Trait{..} = case (M.lookup _traitSpace sx, M.lookup _traitProperty px) of
  (Just s, Just p) -> Right . set traitSpace s $ set traitProperty p t
  (Just _, _) -> Left $ ReferenceError "hydrateTrait" [unPropertyId _traitProperty]
  (_, Just _) -> Left $ ReferenceError "hydrateTrait" [unSpaceId _traitSpace]
  _           -> Left $ ReferenceError "hydrateTrait" [unPropertyId _traitProperty, unSpaceId _traitSpace]

mapRightC :: Monad m => (t -> Either a b) -> ConduitM (Either a t) (Either a b) m ()
mapRightC f = awaitForever $ \ev -> yield $ ev >>= f

sinkFind :: Monad m => (a -> Bool) -> ConduitM (Either e a) Void m (Maybe a)
sinkFind predicate = await >>= \case
  Just (Right a) -> if predicate a then return (Just a) else sinkFind predicate
  Just (Left  _) -> sinkFind predicate
  Nothing        -> return Nothing

sinkMap :: (Ord a, Monad m) => ConduitM (a,b) Void m (Map a b)
sinkMap = sinkList >>= return . M.fromList
