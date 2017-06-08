module Data.Parse
  ( viewer
  , viewSpace
  , at
  , spaces
  , properties
  , traits
  , theorems
  ) where

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

viewer :: MonadStore m => Committish -> m (Either [Error] View)
viewer commish = at commish $ \commit -> do
  (es, ss) <- runConduit $ spaces commit     .| sinkPair
  (ep, ps) <- runConduit $ properties commit .| sinkPair

  (ei, is) <- runConduit $ theorems commit     .| hydrateTheorems ps  .| sinkPair
  (et, ts) <- runConduit $ traits commit ss ps .| hydrateTraits ss ps .| sinkPair

  let errors = es ++ ep ++ ei ++ et
  if length errors > 1
    then return $ Left errors
    else return $ Right $ buildView ss ps ts is (commitVersion commit)

-- TODO: deduplicate this logic
viewSpace :: MonadStore m => SpaceId -> Committish -> m (Either [Error] View)
viewSpace sid commish = at commish $ \commit -> do
  (es, ss) <- runConduit $ spaces     commit .| sinkPair
  (ep, ps) <- runConduit $ properties commit .| sinkPair
  (ei, is) <- runConduit $ theorems   commit .| hydrateTheorems ps .| sinkPair

  case find (\s -> spaceId s == sid) ss of
    Nothing -> return $ Left [NotFound $ "Space " <> tshow sid]
    Just space -> do
      (et, ts) <- runConduit $ traits commit [space] ps .| hydrateTraits [space] ps .| sinkPair
      let errors = es ++ ep ++ ei ++ et
      if length errors > 1
        then return $ Left errors
        else return . Right $ buildView ss ps ts is (commitVersion commit)

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
         -> ConduitM i (Either Error (Theorem Slug)) (ReaderT LgRepo m) ()
theorems commit = sourceCommitEntries commit "theorems"
               .| parseEntry Page.Theorem.parser

traits :: MonadStore m
       => Commit LgRepo
       -> [Space]
       -> [Property]
       -> ConduitM i (Either Error (Trait Slug Slug)) (ReaderT LgRepo m) ()
traits commit ss ps = sourceCommitEntries commit "spaces"
                   .| filterC (\(path, _) -> relevant path)
                   .| parseEntry Page.Trait.parser
                   .| mapC (fmap fst)
  where
    relevant :: TreeFilePath -> Bool
    relevant path = S.member path paths

    -- TODO: split and then check rather than building this whole n*n size set
    paths :: S.Set TreeFilePath
    paths = S.fromList [ (encodeUtf8 $ spaceSlug s) <> "/properties/" <> (encodeUtf8 $ propertySlug p) <> ".md" | s <- ss, p <- ps ]

isReadme :: TreeFilePath -> Bool
isReadme path = "README.md" `BS.isSuffixOf` path

hydrateTheorems :: Monad m
                => [Property]
                -> ConduitM
                   (Either Error (Theorem Slug))
                   (Either Error (Theorem Property))
                   m ()
hydrateTheorems ps = do
  let px = indexBy propertySlug ps
  mapRightC $ mapLeft (ReferenceError "hydrateTheorem") . hydrateTheorem px

hydrateTraits :: Monad m
              => [Space]
              -> [Property]
              -> ConduitM
                   (Either Error (Trait Slug Slug))
                   (Either Error (Trait Space Property))
                   m ()
hydrateTraits ss ps = do
  let sx = indexBy spaceSlug ss
      px = indexBy propertySlug ps
  mapRightC $ hydrateTrait sx px

sourceCommitEntries :: MonadGit LgRepo m
                    => Commit LgRepo
                    -> TreeFilePath
                    -> ConduitM i (TreeFilePath, TreeEntry LgRepo) m ()
sourceCommitEntries commit path = do
  edir <- lift $ do
    tree <- lookupTree $ commitTree commit
    getDir tree path
  either (const $ return ()) sourceTreeEntries edir

parseEntry :: (FromJSON f, MonadStore m)
           => Page.Parser f a
           -> ConduitM
             (TreeFilePath, TreeEntry LgRepo)
             (Either Error a)
             (ReaderT LgRepo m) ()
parseEntry parser = awaitForever $ \(path, entry) -> case entry of
  (BlobEntry _id _) -> do
    blob <- lift $ catBlobUtf8 _id
    yield $ Page.parse parser path blob
  _ -> return ()

sinkPair :: Monad m => ConduitM (Either a b) Void m ([a], [b])
sinkPair = foldMapC f
  where f (Left a ) = ([a], [])
        f (Right b) = ([], [b])

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
