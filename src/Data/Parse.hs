module Data.Parse
  ( viewer
  , cloader
  , findProperty
  , findSpace
  , findTheorem
  , at
  , spaces
  , properties
  , traits
  , trait
  , allTraits
  , theorems
  , spaceEntries
  , propertyEntries
  , traitEntries
  , theoremEntries
  , blobs
  , discardLeftC
  ) where

import           Control.Monad.State.Strict (modify)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.Tagged           (Tagged(..))
import           Git

import Core
import Conduit
import Control.Lens      (set)
import Data.Git          (commitVersion, getDir, lookupCommittish)
import Util

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

unique :: Ord b => (a -> b) -> [a] -> Either Error ()
unique f as = foldM go S.empty as >> return ()
  where
    go acc a =
      let b = f a
      in if S.member b acc
           then Left $ GeneralError "value is not unique"
           else Right $ S.insert b acc


findProperty :: MonadStore m => Committish -> PropertyId -> m (Maybe Property)
findProperty commish _id = do
  CLoader{..} <- loaderAt commish
  runConduit $ clProperties (Just $ S.singleton _id) .| await

findSpace :: MonadStore m => Committish -> SpaceId -> m (Maybe Space)
findSpace commish _id = do
  CLoader{..} <- loaderAt commish
  runConduit $ clSpaces (Just $ S.singleton _id) .| await

findTheorem :: MonadStore m => Committish -> TheoremId -> m (Maybe (Theorem Property))
findTheorem commish _id = do
  CLoader{..} <- loaderAt commish
  mtheorem <- runConduit $ clTheorems (Just $ S.singleton _id) .| await
  case mtheorem of
    Nothing -> return Nothing
    Just theorem -> do
      props <- sourceToList $ clProperties $ Just $ theoremProperties theorem
      case hydrateTheorem (indexBy propertyId props) theorem of
        Left _  -> return Nothing
        Right t -> return $ Just t

viewer :: MonadStore m => Committish -> m (Either [Error] View)
viewer commish = at commish $ \commit -> withErrors $ do
  ss <- noteErrors $ runConduit $ spaces commit .| sinkList
  required $ unique spaceId ss

  ps <- noteErrors $ runConduit $ properties commit .| sinkList
  required $ unique propertyId ps

  is <- noteErrors $ runConduit $ hydratedTheorems commit ps .| sinkList
  required $ unique theoremId is

  ts <- noteErrors $ runConduit $ traits commit ss ps .| sinkList
  required $ unique traitId ts

  return $ V.build ss ps ts is (commitVersion commit)

at :: (MonadStore m, MonadThrow m)
   => Committish
   -> (Commit LgRepo -> m a)
   -> m a
at commish handler =
  lookupCommittish commish >>= \case
    Nothing  -> Core.throwM [CommitNotFound commish]
    Just ref -> lookupCommit (Tagged ref) >>= handler

withPages :: Monad m
          => Page t
          -> (t -> a)
          -> Conduit (TreeFilePath, Text) m (Either LoadError a)
withPages page f = mapC $ \(p,b) -> case Page.parse page (p,b) of
  Left _ -> Left $ LoadError p
  Right v  -> Right $ f v

-- getSource :: (Applicative m, MonadExcept m, MonadBaseControl IO m, MonadIO m)
--           => Tree LgRepo
--           -> TreeFilePath
--           -> ReaderT LgRepo m
--              ( ConduitM () (TreeFilePath, Text) (ReaderT LgRepo m) () )
getSource :: MonadGit r m
          => Tree r
          -> TreeFilePath
          -> m (ConduitM () (TreeFilePath, Text) m ())
getSource tree path = treeEntry tree path >>= \case
  Just (TreeEntry _id) -> do
    sub <- lookupTree _id
    return $ sourceTreeEntries sub .| blobs
  _ -> return $ return ()

loaderAt :: MonadStore m => Committish -> m (CLoader m)
loaderAt commish = at commish $ \commit -> do
  tree <- lookupTree $ commitTree commit
  cloader' tree

cloader :: MonadStore m => TreeT LgRepo m (CLoader m)
cloader = do
  tree   <- currentTree
  loader <- lift $ cloader' tree
  return loader

cloader' :: MonadGit LgRepo m => Tree LgRepo -> m (CLoader m)
cloader' tree = do
  ss <- getSource tree "spaces"
  ps <- getSource tree "properties"
  ts <- getSource tree "theorems"

  let
    inIds :: Ord a => Maybe (Set a) -> a -> Bool
    inIds Nothing _ = True
    inIds (Just xs) x = S.member x xs

    clSpaces ids = ss
              .| filterC (isReadme . fst)
              .| withPages Page.Space.page id
              .| discardLeftC
              .| filterC (\Space{..} -> inIds ids spaceId)

    clProperties ids = ps
                  .| withPages Page.Property.page id
                  .| discardLeftC
                  .| filterC (\Property{..} -> inIds ids propertyId)

    clTheorems ids = ts
                .| withPages Page.Theorem.page id
                .| discardLeftC
                .| filterC (\Theorem{..} -> inIds ids theoremId)

    clTraits sid = do
      let root = "spaces/" <> (encodeUtf8 $ unId sid) <> "/properties"
      source <- getSource tree root
      props  <- sourceToList $ source .| withPages Page.Trait.page (\t -> (_traitProperty t, _traitValue t)) .| discardLeftC
      return $ Right $ M.fromList props

  return $ CLoader{..}

type EntrySource m r = ConduitM () (TreeFilePath, TreeEntry r) m ()

spaceEntries :: MonadGit r m => Commit r -> EntrySource m r
spaceEntries commit = sourceCommitEntries commit "spaces"
                   .| filterC (isReadme . fst)

spaces :: MonadStore m
       => Commit LgRepo
       -> ConduitM () (Either Error Space) m ()
spaces commit = spaceEntries commit .| parseEntry Page.Space.page


propertyEntries :: MonadGit r m => Commit r -> EntrySource m r
propertyEntries commit = sourceCommitEntries commit "properties"

properties :: MonadStore m
           => Commit LgRepo
           -> ConduitM () (Either Error Property) m ()
properties commit = propertyEntries commit .| parseEntry Page.Property.page

theoremEntries :: MonadGit r m => Commit r -> EntrySource m r
theoremEntries commit = sourceCommitEntries commit "theorems"

theorems :: MonadStore m
         => Commit LgRepo
         -> ConduitM () (Either Error (Theorem PropertyId)) m ()
theorems commit = theoremEntries commit .| parseEntry Page.Theorem.page

hydratedTheorems :: MonadStore m
                 => Commit LgRepo
                 -> [Property]
                 -> ConduitM () (Either Error (Theorem Property)) m ()
hydratedTheorems commit ps = theorems commit .| mapRightC hydrate
  where
    hydrate = mapLeft (ReferenceError "hydrateTheorem" . map unId)
            . hydrateTheorem (indexBy propertyId ps)

traitEntries :: MonadGit r m => Commit r -> EntrySource m r
traitEntries commit = sourceCommitEntries commit "spaces"
                   .| filterC (not . isReadme . fst)

allTraits :: MonadStore m => Commit LgRepo -> ConduitM () (Trait SpaceId PropertyId) m ()
allTraits commit = traitEntries commit
                .| parseEntry Page.Trait.page
                .| discardLeftC

trait :: MonadStore m
      => Commit LgRepo
      -> SpaceId
      -> PropertyId
      -> m (Either Error (Trait SpaceId PropertyId))
trait commit sid pid = do
  tree <- lookupTree $ commitTree commit
  let path = "spaces/" <> unId sid <> "/properties/" <> unId pid <> ".md"
  mfile <- treeEntry tree (encodeUtf8 path)
  case mfile of
    Just (BlobEntry oid _) -> do
      blob <- catBlobUtf8 oid
      return $ Page.parse Page.Trait.page (encodeUtf8 path, blob)
    _ -> return . Left $ NotFound path

traits :: MonadStore m
       => Commit LgRepo
       -> [Space]
       -> [Property]
       -> ConduitM () (Either Error (Trait Space Property)) m ()
traits commit ss ps = traitEntries commit
                   .| filterC (\(path, _) -> relevant path)
                   .| parseEntry Page.Trait.page
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
    paths = S.fromList
      [ "spaces/"
      <> (encodeUtf8 $ unId $ spaceId s)
      <> "/properties/"
      <> (encodeUtf8 $ unId $ propertyId p)
      <> ".md"
      | s <- ss, p <- ps
      ]

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
parseEntry page = blobs .| mapC (Page.parse page)

-- TODO: clean this up now that trait is a lens
hydrateTrait :: Map SpaceId s -> Map PropertyId p -> Trait SpaceId PropertyId -> Either Error (Trait s p)
hydrateTrait sx px t@Trait{..} = case (M.lookup _traitSpace sx, M.lookup _traitProperty px) of
  (Just s, Just p) -> Right . set traitSpace s $ set traitProperty p t
  (Just _, _) -> Left $ ReferenceError "hydrateTrait" [unId _traitProperty]
  (_, Just _) -> Left $ ReferenceError "hydrateTrait" [unId _traitSpace]
  _           -> Left $ ReferenceError "hydrateTrait" [unId _traitProperty, unId _traitSpace]
