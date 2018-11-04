module Graph.Client
  ( Client
  , initialize
  , checkout
  , checkoutUserBranch
  , login
  , logout
  , reset
  , run
  ) where

import Test.Import hiding (login, logout, reset)

import           Control.Lens         hiding ((.=))
import           Data.Aeson           (Result(..), ToJSON(..), Value(..), fromJSON)
import           Data.Aeson.Lens
import           Data.Char            (toLower)
import qualified Data.HashMap.Strict  as HM
import qualified Data.Map             as M
import qualified Data.Text            as T
import           Database.Persist.Sql (Entity(..))

import qualified Data.Branch         as Branch
import           Util                (findOrCreate)
import qualified Graph.Root          as Graph
import           Graph.Types         (Operation(..), Name(..), QueryData(..))
import           Graph.Schema        (PatchInput(..))

data Client = Client
  { user       :: IORef (Maybe (Entity User))
  , patch      :: IORef PatchInput
  , savepoints :: IORef (Map BranchName Sha)
  }

initialize :: TestM Client
initialize = do
  (Entity _ base) <- Branch.ensureBaseBranch
  sha <- Branch.headSha base

  Client
    <$> newIORef Nothing
    <*> newIORef (PatchInput (branchName base) (Just sha))
    <*> newIORef (M.fromList [(branchName base, sha)])

login :: Client -> User -> TestM (Entity User)
login Client{..} u = do
  entity <- findOrCreate (UniqueUser . userEmail) u
  _ <- Branch.ensureUserBranch entity
  writeIORef user $ Just entity
  return entity

logout :: Client -> TestM ()
logout Client{..} = writeIORef user Nothing

checkout :: Client -> Branch -> TestM ()
checkout Client{..} branch = do
  sha <- Branch.headSha branch
  writeIORef patch $ PatchInput
    { branch = branchName branch
    , sha    = Just sha
    }
  modifyIORef' savepoints $ M.alter (maybe (Just sha) Just) (branchName branch)

checkoutUserBranch :: Client -> TestM (Entity Branch)
checkoutUserBranch client@Client{..} = do
  cu <- readIORef user >>= \case
    Nothing -> notFound "User" ("current" :: Text)
    Just eu -> return eu

  eb@(Entity _ branch) <- Branch.ensureUserBranch cu
  checkout client branch
  return eb

run :: Client -> Name -> Value -> TestM Value
run Client{..} name vs = do
  p <- readIORef patch

  let
    Success variables = fromJSON $ addVariable vs "patch" p
    operation = Operation $ Just name
    req = QueryData operation "" variables

  u   <- readIORef user
  env <- view foundation
  res <- toJSON <$> Graph.run env u req

  let dat = fromMaybe Null $ res ^? key "data" . _Value

  writeIORef patch $ p { sha = Just $ dat ^. key (operationKey operation) . key "version" . _String }

  return dat

reset :: Client -> TestM ()
reset Client{..} = do
  points <- readIORef savepoints
  forM_ (M.toList points) $ \(name, sha) -> do
    branch <- Branch.byName name
    Branch.reset branch $ CommitSha sha

addVariable :: ToJSON v => Value -> Text -> v -> Value
addVariable (Object m) name val = Object $ m <> HM.fromList [(name, toJSON val)]
addVariable val _ _ = val

operationKey :: Operation -> Text
operationKey (Operation (Just (Name name))) = case T.uncons name of
  Just (c, cs) -> toLower c `T.cons` cs
  Nothing -> panic "operationKey: empty name"
operationKey _ = panic "operationKey: no name"
