module Persist.Backend.Git
  ( Config(..)
  , Env(..)
  , Page
  , Pagable
  , Paths
  , initialize
  , run
  --
  , branches
  , createBranch
  , createMaster
  , find
  , head
  , history
  , parseId
  , paths
  , scan
  , system
  , write
  ) where

import Core hiding (find, head)

import           Conduit
import           Control.Monad.Fail       (fail)
import           Data.Attoparsec.Text     (Parser, parseOnly)
import           Data.Time.LocalTime      (getZonedTime)
import qualified Data.Id                  as Id
import           Data.Tagged              (Tagged(..))
import qualified Data.Text                as Text
import           Git                      hiding (Commit, currentTree)
import qualified Git
import           Git.Libgit2              (HasLgRepo(..), LgRepo, OidPtr, openLgRepository, shaToOid)
import           Persist.Backend.Git.Page (Pagable, Page)
import qualified Persist.Backend.Git.Page as Page

data Config = Config
  { repoPath :: FilePath
  } deriving (Generic, Show, Eq)

data Env = Env
  { config     :: Config
  , repository :: LgRepo
  }

data Paths i = Paths
  { root   :: Text
  , format :: i -> Text
  , parser :: Parser i
  }

newtype Git a = Git
  { unGit :: ReaderT Env IO a
  } deriving (Generic, Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadUnliftIO, MonadMask, MonadCatch, MonadThrow)

instance HasLgRepo Git where
  getRepository = asks repository

initialize :: MonadIO m => Config -> m Env
initialize config@Config{..} = do
  repository <- liftIO $ openLgRepository $ RepositoryOptions
      { repoPath
      , repoWorkingDir = Nothing
      , repoIsBare     = True
      , repoAutoCreate = True
      }

  return Env{..}

history :: MonadIO m => Env -> Branch -> m [Commit]
history env branch = run env $ runConduit $ log branch .| sinkList

branches :: Git [Branch]
branches = map (Branch . Text.replace "refs/heads/" "") <$> Git.listReferences

--

run :: MonadIO m => Env -> Git a -> m a
run env action = liftIO $ runReaderT (unGit action) env

paths :: Text -> (i -> Text) -> Parser i -> Paths i
paths r f p = Paths r (\id -> r <> "/" <> f id) p

parseId :: Id.Encodable a => Text -> Parser (Id a)
parseId txt = case Id.decode txt of
  Left err -> fail $ Text.unpack err
  Right id -> return id

scan :: Pagable a
     => Page a i
     -> Paths i
     -> Branch
     -> Git [a Identity]
scan page Paths{..} branch =
  currentTree branch >>= cd root >>= \case
    Just tree -> runConduit $
      sourceTreeEntries tree
      .| parseC parser (Page.parse page)
      .| sinkList
    _ -> return []
  where
    cd :: Text -> Tree LgRepo -> Git (Maybe (Tree LgRepo))
    cd part tree = treeEntry tree (encodeUtf8 part) >>= \case
      Just (TreeEntry oid) -> Just <$> lookupTree oid
      _ -> return Nothing

-- TODO: may want to pass these downstream as parse errors
parseC :: Parser i
       -> (Text -> Either e a)
       -> ConduitM (TreeFilePath, TreeEntry LgRepo) a Git ()
parseC id page = awaitForever $ \(path, entry) ->
  case (parseOnly id (decodeUtf8 path), entry) of
    (Right _, BlobEntry oid _) -> do
      content <- lift $ catBlobUtf8 oid
      case page content of
        Left _ -> return ()
        Right parsed -> yield parsed
    _ -> return ()

find :: Pagable a
     => Page a i
     -> Paths i
     -> Branch
     -> i
     -> Git (Maybe (a Identity))
find page Paths{..} branch id = do
  let path = encodeUtf8 $ format id
  tree <- currentTree branch
  treeEntry tree path >>= \case
    Just (BlobEntry oid _) -> do
      blob <- catBlobUtf8 oid
      case Page.parse page blob of
        Right val -> return $ Just val
        _         -> return Nothing
    _ -> return Nothing

write :: Pagable a
      => Page a i
      -> Paths i
      -> Branch
      -> Commit
      -> i
      -> (a Identity)
      -> Git ()
write page Paths{..} branch message _ object = void $ commit message branch $
  pages
    [ ( encodeUtf8 $ format $ object ^. Page.id page
      , Page.write page object
      )
    ]

commit :: Commit
       -> Branch
       -> TreeT LgRepo (ReaderT LgRepo IO) a
       -> Git (Git.Commit LgRepo)
commit Commit{..} branch action = do
  (ref, parent)       <- head' branch
  tree                <- lookupTree $ commitTree parent
  (_, next)           <- runRepo $ withTree tree action
  (author, committer) <- sign user

  createCommit
    [commitOid parent]
    next
    author
    committer
    message
    (Just ref)

pages :: [(TreeFilePath, Text)] -> TreeT LgRepo (ReaderT LgRepo IO) ()
pages = mapM_ $ \(path, contents) ->
  (lift $ createBlobUtf8 contents) >>= putBlob path

sign :: User -> Git (Signature, Signature)
sign user = do
  time <- liftIO getZonedTime
  return
    ( defaultSignature
      { signatureEmail = userEmail user
      , signatureName  = userName user
      , signatureWhen  = time
      }
    , defaultSignature
      { signatureEmail = userEmail system
      , signatureName  = userName system
      , signatureWhen  = time
      }
    )

head :: Branch -> Git Version
head = head' >=> \(_, c) -> return $ Version $ show $ commitOid c

head' :: Branch -> Git (RefName, Git.Commit LgRepo)
head' branch = do
  ref <- asks $ \Env{..} -> rh branch
  let
    go = resolveReference ref >>= \case
      Just oid -> do
        cmt <- lookupCommit $ Tagged oid
        return (ref, cmt)
      _ -> do
        master >>= createReference ref
        go
  go

createBranch :: Branch -> Branch -> Git ()
createBranch b f =
  lookupReference (rh f) >>= \case
    Just refTarget -> createReference (rh b) refTarget
    _ -> liftIO $ putStrLn ("Could not find base branch " <> branchName f) -- TODO: handle error

currentTree :: Branch -> Git (Tree LgRepo)
currentTree branch =
  head' branch >>= lookupTree . commitTree . snd

log :: Branch -> ConduitM () Commit Git ()
log branch = lift (head' branch) >>= go . snd
  where
    go c = do
      let Signature{..} = commitAuthor c
      yield $ Commit (User signatureName signatureEmail False) (commitLog c)
      parents <- lift $ lookupCommitParents c
      case parents of
        (parent : _) -> go parent
        _ -> return ()

rh :: Branch -> Text
rh b = "refs/heads/" <> branchName b

createMaster :: Branch -> Git ()
createMaster b = do
  -- If we don't have branch literally named `master`, something fails in
  -- packed-refs. It may be possible to handle this better in `initialize` but
  -- for now, we'll make sure the nominal master branch exists, and then create
  -- the logical master branch off of it.
  void $ master
  createBranch b $ Branch "master"

-- TODO: parametrize `master`
master :: Git (RefTarget LgRepo)
master = lookupReference "refs/heads/master" >>= \case
  Just target -> return target
  _ -> do
    (author, committer) <- sign system
    oid <- emptyTreeOid

    void $ createCommit
      []
      oid
      author
      committer
      "Initial commit"
      (Just "refs/heads/master")

    master

emptyTreeOid :: Git (Tagged (Tree LgRepo) OidPtr)
emptyTreeOid = do
  sha <- textToSha "4b825dc642cb6eb9a060e54bf8d69288fbee4904"
  oid <- liftIO $ shaToOid sha
  return $ Tagged oid

runRepo :: ReaderT LgRepo IO a -> Git a
runRepo action =
  asks repository >>= liftIO . runReaderT action

system :: User
system = User
  { userName       = "Pi-Base"
  , userEmail      = "hausdorff@pi-base.org"
  , userIsReviewer = True
  }
