module Data
  ( mkStore
  , storeMaster
  , parseViewer
  , viewerAtRef
  , fetchPullRequest
  , updatedPages
  , makeId
  , slugify
  --
  , assertTrait
  , assertTheorem
  ) where

import           Control.Lens
import qualified Data.Map.Strict as M
import qualified Data.UUID       as UUID
import qualified Data.UUID.V4    as UUID
import           System.Process  (callCommand)

import qualified Data.Parse as P
import qualified Logic      as L
import qualified View       as V

import qualified Page.Parser
import qualified Page.Theorem
import qualified Page.Trait

import Core     hiding (assert)
import Data.Git (openRepo, useRepo, writeContents)
import Util     (fetch)

storeMaster :: MonadStore m
            => m (Either [Error] View)
storeMaster = storeCached $ parseViewer (CommitRef $ Ref "master")

fetchPullRequest :: MonadIO m => FilePath -> m ()
fetchPullRequest path = liftIO $ do
  putStrLn $ "Pulling updates for repo"
  callCommand $ "cd " ++ path ++ " && git fetch origin"

viewerAtRef :: MonadStore m => Text -> m (Either [Error] View)
viewerAtRef = parseViewer . CommitRef . Ref

parseViewer :: MonadStore m
            => Committish
            -> m (Either [Error] View)
parseViewer commish = do
  eviewer <- P.viewer commish
  -- validate
  return eviewer

makeId :: MonadIO m => (Text -> a) -> Text -> m a
makeId constructor prefix = do
  uuid <- liftIO UUID.nextRandom
  return . constructor $ prefix <> UUID.toText uuid

slugify :: Text -> Text
slugify t = t -- TODO

userBranchRef :: User -> Committish
userBranchRef User{..} = CommitRef . Ref $ "users/" <> userName

assertTrait :: MonadStore m => User -> Trait SpaceId PropertyId -> m (Either [Error] View)
assertTrait user trait = assert L.assertTrait trait user (P.viewSpace $ traitSpace trait) $ \_v ->
  -- TODO: better commit message
  "Add trait " <> tshow (traitSpace trait, traitProperty trait)

assertTheorem :: MonadStore  m => User -> Theorem PropertyId -> m (Either [Error] View)
assertTheorem user theorem = assert L.assertTheorem theorem user P.viewer $ \_v ->
 -- TODO: better commit message
 "Add theorem " <> (unTheoremId $ theoremId theorem)

assert :: MonadStore m
       => (a -> L.Logic ())
       -> a
       -> User
       -> (Committish -> m (Either [Error] View))
       -> (View -> Text)
       -> m (Either [Error] View)
assert logic obj user getView message = getView (userBranchRef user) >>= \case
  Left errs -> return $ Left errs
  Right v -> case L.updates v $ logic obj of
    Left errs -> return $ Left [LogicError errs]
    Right updates -> persistUpdates updates user (message v)

buildTheorem :: MonadStore m
             => View -> Formula PropertyId -> Formula PropertyId -> Text -> m (Theorem Property)
buildTheorem v ant con desc = do
  _id <- makeId TheoremId "t"
  let dehyrdated = Theorem _id (Implication ant con) Nothing desc
  case hydrateTheorem (v ^. viewProperties) dehyrdated of
    Left errs -> throwM . NotFound $ tshow errs
    Right t   -> return t

buildTrait :: MonadThrow m
           => View -> SpaceId -> PropertyId -> Bool -> Text -> m (Trait Space Property)
buildTrait View{..} sid pid value description = do
  space    <- fetch sid _viewSpaces
  property <- fetch pid _viewProperties
  return $ Trait space property value description

updatedPages :: View -> [(TreeFilePath, Text)]
updatedPages v = map (Page.Parser.write . Page.Theorem.write) (V.theorems v)
              <> map (Page.Parser.write . Page.Trait.write)   (V.traits   v)

persistUpdates :: MonadStore m => View -> User -> Text -> m (Either [Error] View)
persistUpdates v user message = useRepo $
  writeContents user message (updatedPages v) >>=
    return . fmap (\version -> v { _viewVersion = Just version })

mkStore :: FilePath -> IO Store
mkStore path = Store
  <$> openRepo path
  <*> newMVar Nothing

storeCached :: MonadStore m
            => m (Either a View)
            -> m (Either a View)
storeCached f = do
  Store{..} <- getStore
  modifyMVar storeCache $ \mev -> case mev of
    Just viewer -> return $ (Just viewer, Right viewer)
    _ -> f >>= \case
      Left err     -> return $ (Nothing, Left err)
      Right viewer -> return $ (Just viewer, Right viewer)

