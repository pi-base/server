module Data.Parse
  ( viewer
  , viewSpace
  , at
  , spaces
  , properties
  , traits
  , theorems
  ) where

import           Control.Monad.State.Strict (modify)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import qualified Data.Set              as S
import           Data.Tagged           (Tagged(..))
import           Git

import Core     hiding (groupBy)
import Conduit
import Data.Git (commitVersion, getDir, lookupCommitish, useRepo)
import Util     (groupBy, indexBy)

import qualified Page
import qualified Page.Property
import qualified Page.Space
import qualified Page.Theorem
import qualified Page.Trait

type Slug = Text

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


buildView :: [Space]
          -> [Property]
          -> [Trait Space Property]
          -> [Theorem Property]
          -> Version
          -> View
buildView ss ps ts is version = View
  { _viewSpaces     = indexBy spaceId ss
  , _viewProperties = indexBy propertyId ps
  , _viewTraits     = M.map (indexBy traitProperty) $ groupBy traitSpace $ map identifyTrait ts
  , _viewTheorems   = indexBy theoremId $ map (fmap propertyId) is
  , _viewProofs     = mempty
  , _viewVersion    = Just version
  }

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

  return $ buildView [s] ps ts is (commitVersion commit)

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

  return $ buildView ss ps ts is (commitVersion commit)

at :: MonadStore m
   => Committish
   -> (Commit LgRepo -> ReaderT LgRepo m (Either [Error] a))
   -> m (Either [Error] a)
at commish handler = useRepo $
  lookupCommitish commish >>= \case
    Nothing  -> return $ Left [CommitNotFound commish]
    Just ref -> (lookupCommit $ Tagged ref) >>= handler

spaces :: MonadStore m
       => Commit LgRepo
       -> ConduitM i (Either Error Space) (ReaderT LgRepo m) ()
spaces commit = sourceCommitEntries commit "spaces"
             .| filterC (\(path, _) -> isReadme path)
             .| parseEntry Page.Space.parser

properties :: MonadStore m
           => Commit LgRepo
           -> ConduitM i (Either Error Property) (ReaderT LgRepo m) ()
properties commit = sourceCommitEntries commit "properties"
                 .| parseEntry Page.Property.parser

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

traits :: MonadStore m
       => Commit LgRepo
       -> [Space]
       -> [Property]
       -> ConduitM i (Either Error (Trait Space Property)) (ReaderT LgRepo m) ()
traits commit ss ps = sourceCommitEntries commit "spaces"
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

parseEntry :: (FromJSON f, MonadStore m, MonadGit r m)
           => Page.Parser f a
           -> ConduitM
             (TreeFilePath, TreeEntry r)
             (Either Error a)
             m ()
parseEntry parser = awaitForever $ \(path, entry) -> case entry of
  (BlobEntry _id _) -> do
    blob <- lift $ catBlobUtf8 _id
    yield $ Page.parse parser path blob
  _ -> return ()

hydrateTrait :: Map Text s -> Map Text p -> Trait Text Text -> Either Error (Trait s p)
hydrateTrait sx px t@Trait{..} = case (M.lookup traitSpace sx, M.lookup traitProperty px) of
  (Just s, Just p) -> Right $ t { traitSpace = s, traitProperty = p }
  (Just _, _) -> Left $ ReferenceError "hydrateTrait" [traitProperty]
  (_, Just _) -> Left $ ReferenceError "hydrateTrait" [traitSpace]
  _           -> Left $ ReferenceError "hydrateTrait" [traitProperty, traitSpace]

mapRightC :: Monad m => (t -> Either a b) -> ConduitM (Either a t) (Either a b) m ()
mapRightC f = awaitForever $ \ev -> yield $ ev >>= f

identifyTrait :: Trait Space Property -> Trait SpaceId PropertyId
identifyTrait t@Trait{..} = t { traitSpace = spaceId traitSpace, traitProperty = propertyId traitProperty }
