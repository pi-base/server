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
import Data.Git (commitVersion, getDir, lookupCommittish, useRepo)
import Util     (indexBy)

import qualified Page
import qualified Page.Parser
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

spaceEntries commit = sourceCommitEntries commit "spaces"
                   .| filterC (isReadme . fst)

spaces :: MonadStore m
       => Commit LgRepo
       -> ConduitM i (Either Error Space) (ReaderT LgRepo m) ()
spaces commit = spaceEntries commit .| parseEntry Page.Space.parser


propertyEntries commit = sourceCommitEntries commit "properties"

properties :: MonadStore m
           => Commit LgRepo
           -> ConduitM i (Either Error Property) (ReaderT LgRepo m) ()
properties commit = propertyEntries commit .| parseEntry Page.Property.parser

theorems :: MonadStore m
         => Commit LgRepo
         -> [Property]
         -> ConduitM i (Either Error (Theorem Property)) (ReaderT LgRepo m) ()
theorems commit ps = sourceCommitEntries commit "theorems"
                  .| parseEntry Page.Theorem.parser
                  .| mapRightC hydrate
  where
    px      = indexBy propertySlug ps
    hydrate = mapLeft (ReferenceError "hydrateTheorem") . hydrateTheorem px

traitEntries commit = sourceCommitEntries commit "spaces"
                   .| filterC (not . isReadme . fst)

traits :: MonadStore m
       => Commit LgRepo
       -> [Space]
       -> [Property]
       -> ConduitM i (Either Error (Trait Space Property)) (ReaderT LgRepo m) ()
traits commit ss ps = traitEntries commit
                   .| filterC (\(path, _) -> relevant path)
                   .| parseEntry Page.Trait.parser
                   .| mapC (fmap fst)
                   .| mapRightC (hydrateTrait sx px)
  where
    sx :: M.Map Text Space
    sx = indexBy spaceSlug ss

    px :: M.Map Text Property
    px = indexBy propertySlug ps

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
  either (const $ return ()) sourceTreeEntries edir

blobs :: MonadGit r m => ConduitM (TreeFilePath, TreeEntry r) (TreeFilePath, Text) m ()
blobs = awaitForever $ \(path, entry) -> case entry of
  (BlobEntry _id _) -> do
    blob <- lift $ catBlobUtf8 _id
    yield (path, blob)
  _ -> return ()

parseEntry :: (FromJSON f, MonadStore m, MonadGit r m)
           => Page.Parser f a
           -> ConduitM
             (TreeFilePath, TreeEntry r)
             (Either Error a)
             m ()
parseEntry parser = blobs .| mapC (\(path, blob) -> Page.parse parser path blob)

hydrateTrait :: Map Text s -> Map Text p -> Trait Text Text -> Either Error (Trait s p)
hydrateTrait sx px t@Trait{..} = case (M.lookup traitSpace sx, M.lookup traitProperty px) of
  (Just s, Just p) -> Right $ t { traitSpace = s, traitProperty = p }
  (Just _, _) -> Left $ ReferenceError "hydrateTrait" [traitProperty]
  (_, Just _) -> Left $ ReferenceError "hydrateTrait" [traitSpace]
  _           -> Left $ ReferenceError "hydrateTrait" [traitProperty, traitSpace]

mapRightC :: Monad m => (t -> Either a b) -> ConduitM (Either a t) (Either a b) m ()
mapRightC f = awaitForever $ \ev -> yield $ ev >>= f

sinkFind :: Monad m => (a -> Bool) -> ConduitM (Either e a) Void m (Maybe a)
sinkFind predicate = await >>= \case
  Just (Right a) -> if predicate a then return (Just a) else sinkFind predicate
  Just (Left  _) -> sinkFind predicate
  Nothing        -> return Nothing

sinkMap :: (Ord a, Monad m) => ConduitM (a,b) Void m (Map a b)
sinkMap = sinkList >>= return . M.fromList
