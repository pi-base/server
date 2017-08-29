module Data.Parse
  ( viewer
  , viewSpace
  , loader
  , cloader
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
  , findTheorem
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

import Git.Libgit2.Types (MonadExcept)

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

-- TODO: now that the file paths are determined by the object ids,
--   these don't need to load more than just the one blob
-- findTheorem in particular is _wildly_ inefficient
findProperty :: MonadStore m => Committish -> PropertyId -> m (Maybe Property)
findProperty commish pid = at commish $ \commit ->
  runConduit $ properties commit .| sinkFind (\p -> propertyId p == pid)

findSpace :: MonadStore m => Committish -> SpaceId -> m (Maybe Space)
findSpace commish sid = at commish $ \commit ->
  runConduit $ spaces commit .| sinkFind (\s -> spaceId s == sid)

findTheorem :: MonadStore m => Committish -> TheoremId -> m (Maybe (Theorem Property))
findTheorem commish tid = do
  ev <- viewSpace (SpaceId "S000001") commish
  case ev of
    Left  _ -> return Nothing
    Right v -> case M.lookup tid $ _viewTheorems v of
      Nothing -> return Nothing
      Just t  -> case hydrateTheorem (_viewProperties v) t of
        Left _ -> return Nothing
        Right t' -> return $ Just t'

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

failFast :: Monad m => ConduitM (Either a b) Void m (Either a [b])
failFast = go []
  where
    go acc = await >>= \case
      Just (Left a)  -> return $ Left a
      Just (Right v) -> go $ v : acc
      Nothing        -> return $ Right acc

withPages page f = mapC $ \(p,b) -> case Page.parse page (p,b) of
  Left err -> Left $ LoadError p
  Right v  -> Right $ f v

loader :: (Monad m, Monad n) => m (Loader n)
loader = do
  error "FIXME: replace with cloader"
  -- tree <- currentTree
  -- let
  --   -- TODO: clean up
  --   loadImplications :: MonadStore m => GitT m (Either LoadError [(TheoremId, Implication PropertyId)])
  --   loadImplications = lift $ runConduit $ subtreeEntries tree "theorems"
  --                                              .| withPages Page.Theorem.page (\t -> (theoremId t, theoremImplication t))
  --                                              .| failFast

  --   loadSpaceIds :: MonadStore m => GitT m (Either LoadError [SpaceId])
  --   loadSpaceIds = lift $ runConduit $ subtreeEntries tree "spaces"
  --                                          .| filterC (isReadme . fst)
  --                                          .| withPages Page.Space.page (\s -> spaceId s)
  --                                          .| failFast

  --   loadSpace :: MonadStore m => SpaceId -> GitT m (Either LoadError Properties)
  --   loadSpace sid = lift $ do
  --     epairs <- runConduit $ subtreeEntries tree ("spaces/" <> (encodeUtf8 $ unSpaceId sid) <> "/properties")
  --                         .| withPages Page.Trait.page (\t -> (_traitProperty t, _traitValue t))
  --                         .| failFast
  --     case epairs of
  --       Left err    -> return $ Left err
  --       Right pairs -> return . Right $ M.fromList pairs

  --   loadSpaces :: MonadStore m => Set SpaceId -> GitT m (Either LoadError [Space])
  --   loadSpaces ids = lift $ runConduit $ subtreeEntries tree "spaces"
  --                                            .| filterC (isReadme . fst)
  --                                            .| withPages Page.Space.page id
  --                                            .| filterC matches
  --                                            .| failFast
  --     where
  --       matches (Left _) = True
  --       matches (Right s) = S.member (spaceId s) ids

  --   loadProperties :: MonadStore m => Set PropertyId -> GitT m (Either LoadError [Property])
  --   loadProperties ids = lift $ runConduit $ subtreeEntries tree "properties"
  --                                            .| withPages Page.Property.page id
  --                                            .| filterC matches
  --                                            .| failFast
  --     where
  --       matches (Left _) = True
  --       matches (Right p) = S.member (propertyId p) ids

  -- return $ Loader
  --   { loaderSpace        = loadSpace
  --   , loaderSpaceIds     = loadSpaceIds
  --   , loaderSpaces       = loadSpaces
  --   , loaderProperties   = loadProperties
  --   , loaderImplications = loadImplications
  --   }

getSource :: (Applicative m, MonadExcept m, MonadBaseControl IO m, MonadIO m)
          => Tree LgRepo
          -> TreeFilePath
          -> ReaderT LgRepo m
             ( ConduitM () (TreeFilePath, Text) (ReaderT LgRepo m) () )
getSource tree path = treeEntry tree path >>= \case
  Just (TreeEntry _id) -> do
    sub <- lookupTree _id
    return $ sourceTreeEntries sub .| blobs
  Nothing -> return $ return ()

cloader :: MonadStore m => TreeT LgRepo (ReaderT LgRepo m) (CLoader (ReaderT LgRepo m))
cloader = do
  tree <- currentTree
  ss   <- lift $ getSource tree "spaces"
  ps   <- lift $ getSource tree "properties"
  ts   <- lift $ getSource tree "theorems"

  let
    inIds :: Ord a => Maybe (Set a) -> a -> Bool
    inIds Nothing x = True
    inIds (Just xs) x = S.member x xs

    -- spaces :: MonadStore m => Maybe (Set SpaceId) -> Source m Space
    spaces ids = ss
              .| filterC (isReadme . fst)
              .| withPages Page.Space.page id
              .| discardLeftC
              .| filterC (\Space{..} -> inIds ids spaceId)

    properties ids = ps
                  .| withPages Page.Property.page id
                  .| discardLeftC
                  .| filterC (\Property{..} -> inIds ids propertyId)

    theorems ids = ts
                .| withPages Page.Theorem.page id
                .| discardLeftC
                .| filterC (\Theorem{..} -> inIds ids theoremId)

    traits sid = do
      let root = "spaces/" <> (encodeUtf8 $ unSpaceId sid) <> "/properties"
      source <- getSource tree root
      ts     <- sourceToList $ source .| withPages Page.Trait.page (\t -> (_traitProperty t, _traitValue t)) .| discardLeftC
      return $ Right $ M.fromList ts

  return $ CLoader
    { clSpaces     = spaces
    , clProperties = properties
    , clTheorems   = theorems
    , clTraits     = traits
    }

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

subtreeEntries :: MonadGit LgRepo m
               => Tree LgRepo -> TreeFilePath -> ConduitM i (TreeFilePath, Text) m ()
subtreeEntries tree path = (lift $ getDir tree path) >>= \case
  Left _   -> return ()
  Right st -> sourceTreeEntries st .| mapC (\(p,t) -> (path <> "/" <> p, t)) .| blobs

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
parseEntry page = blobs .| mapC (Page.parse page)

-- TODO: clean this up now that trait is a lens
hydrateTrait :: Map SpaceId s -> Map PropertyId p -> Trait SpaceId PropertyId -> Either Error (Trait s p)
hydrateTrait sx px t@Trait{..} = case (M.lookup _traitSpace sx, M.lookup _traitProperty px) of
  (Just s, Just p) -> Right . set traitSpace s $ set traitProperty p t
  (Just _, _) -> Left $ ReferenceError "hydrateTrait" [unPropertyId _traitProperty]
  (_, Just _) -> Left $ ReferenceError "hydrateTrait" [unSpaceId _traitSpace]
  _           -> Left $ ReferenceError "hydrateTrait" [unPropertyId _traitProperty, unSpaceId _traitSpace]

mapRightC :: Monad m => (t -> Either a b) -> ConduitM (Either a t) (Either a b) m ()
mapRightC f = awaitForever $ \ev -> yield $ ev >>= f

discardLeftC :: Monad m => ConduitM (Either a b) b m ()
discardLeftC = awaitForever $ either (const $ return ()) yield

sinkFind :: Monad m => (a -> Bool) -> ConduitM (Either e a) Void m (Maybe a)
sinkFind predicate = await >>= \case
  Just (Right a) -> if predicate a then return (Just a) else sinkFind predicate
  Just (Left  _) -> sinkFind predicate
  Nothing        -> return Nothing

sinkMap :: (Ord a, Monad m) => ConduitM (a,b) Void m (Map a b)
sinkMap = sinkList >>= return . M.fromList
