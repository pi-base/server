{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Core hiding (option)

import           Control.Lens.Setter         hiding (argument)
import qualified Data.Map                    as M
import qualified Data.Text                   as T
import           LoadEnv                     (loadEnvFrom)
import           Options.Applicative
import qualified Shelly                      as Sh
import           System.IO                   (BufferMode(..), hSetBuffering)

import qualified Auth
import qualified Build
import qualified Config
import qualified Config.Boot         as Config (boot)
import qualified Data.Branch         as Branch
import qualified Data.Branch.Move    as Branch
import qualified Data.Branch.Merge   as Branch
import qualified Data.Store          as Store
import qualified Logger              as Logger
import qualified Graph.Queries.Cache as Graph
import qualified Server
import           Server.Class        (AppM, runApp)

data Cli = Cli
  { cmd    :: Command
  , config :: Settings
  }

opts :: Settings -> ParserInfo Cli
opts s = info ((Cli <$> commandsP s <*> configP s) <**> helper) idm

configP :: Settings -> Parser Settings
configP s = do
  logLevel' <- option (eitherReader Logger.parseLogLevel)
    ( long "verbosity"
    <> short 'v'
    <> metavar "LOG_LEVEL"
    <> help "Logger verbosity level"
    <> value (s ^. logLevel)
    )

  port' <- option auto
    ( long "port"
    <> metavar "PORT"
    <> help "Port to listen on"
    <> value (s ^. port)
    <> showDefault
    )

  repoPath' <- option str
    ( long "repo"
    <> metavar "REPO"
    <> help "Path to working repo"
    <> value (s ^. repoSettings . Store.repoPath)
    <> showDefault
    )

  testMode' <- flag (s ^. testMode) True
    ( long "test"
    <> help "Enable integration test mode"
    <> showDefault
    )

  return $ s
    & logLevel .~ logLevel'
    & port .~ port'
    & testMode .~ testMode'
    & repoSettings . Store.repoPath .~ repoPath'

data Command
  = BranchCmd BranchCommand
  | SchemaCmd
  | SettingsCmd
  | Server
  | UserCmd UserCommand
  | VersionCmd

commandsP :: Settings -> Parser Command
commandsP s = subparser $ mconcat
  [ command "branch"
    ( info (BranchCmd <$> branchCommandP (s ^. repoSettings) <**> helper) $
      progDesc "Operate on a branch"
    )
  , command "schema"
    ( info (pure SchemaCmd) $
      progDesc "Print GraphQL schema"
    )
  , command "server"
    ( info (pure Server) $
      progDesc "Start the server"
    )
  , command "settings"
    ( info (pure SettingsCmd) $
      progDesc "Display the current settings"
    )
  , command "users"
    ( info (UserCmd <$> userCommandP <**> helper) $
      progDesc "Manage users"
    )
  , command "version"
    ( info (pure VersionCmd) $
      progDesc "Display version information"
    )
  ]

data BranchCommand
  = BranchList
  | BranchMove     Move
  | BranchMerge    Merge
  | BranchValidate Validate

branchCommandP :: Store.Settings -> Parser BranchCommand
branchCommandP s = subparser $ mconcat
  [ command "ls"
    ( info (pure BranchList) $
      progDesc "List branches"
    )
  , command "mv"
    ( info (BranchMove <$> mvP <**> helper) $
      progDesc "Rename files"
    )
  , command "merge"
    ( info (BranchMerge <$> mergeP s <**> helper) $
      progDesc "Merge branches"
    )
  , command "validate"
    ( info (BranchValidate <$> validateP <**> helper) $
      progDesc "Validate a branch"
    )
  ]

data Move = Move
  { branch  :: BranchName
  , message :: Text
  , updates :: [Text]
  }

mvP :: Parser Move
mvP = Move
  <$> option str
      ( long "branch"
      <> metavar "BRANCH"
      <> help "Branch to update"
      )
  <*> option str
      ( long "message"
      <> metavar "MESSAGE"
      <> help "Text of commit message"
      )
  <*> some
      ( argument str
        ( metavar "UPDATES..."
        <> help "Space-separated list of FROM TO pairs"
        )
      )

data Merge = Merge
  { from    :: Text
  , into    :: Text
  , message :: Text
  }

mergeP :: Store.Settings -> Parser Merge
mergeP settings = Merge
  <$> option str
      ( long "from"
      <> help "Branch to merge from"
      )
  <*> option str
      ( long "into"
      <> help "Branch to merge into"
      <> showDefault
      <> value (settings ^. Store.defaultBranch)
      )
  <*> option str
      ( long "message"
      <> short 'm'
      <> help "Full commit message text"
      )

data UserCommand
  = UserUAT

userCommandP :: Parser UserCommand
userCommandP = subparser $ mconcat
  [ command "uat"
    ( info (pure UserUAT) $
      progDesc "Create token for UAT user"
    )
  ]

data Validate = Validate
  { branch :: Text
  }

validateP :: Parser Validate
validateP = Validate
  <$> argument str (metavar "BRANCH" <> help "Branch to check")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  loadEnvFrom ".env"
  defaults <- Config.server
  Cli{..}  <- execParser $ opts defaults

  let
    h :: forall a. AppM a -> IO a
    h m = do
        env    <- Config.boot config
        result <- runApp env m
        Logger.flush $ env ^. envFoundation . appLogger
        return result

  case cmd of
    BranchCmd b -> h $ case b of
      BranchList -> do
        void $ Branch.claimUserBranches
        branches <- Branch.all

        lines <- forM branches $ \br -> do
          sha <- Branch.headSha br
          return $ branchName br <> " @ " <> sha

        putStrLn ""
        mapM_ putStrLn $ sort lines

      -- mv files on a branch, while keeping references consistent
      BranchMove Move{..} -> do
        meta <- getCommitMeta message
        Branch.moveIds branch meta $ gatherUpdates M.empty updates
        where
          gatherUpdates :: Map Uid Uid -> [Text] -> Map Uid Uid
          gatherUpdates acc [] = acc
          gatherUpdates acc (from : to : rest) = gatherUpdates (M.insert from to $ acc) rest
          gatherUpdates _ _ = panic "Received odd number of updates"

      -- merge one branch into another, collapsing ids
      BranchMerge Merge{..} -> void $ do
        f <- Branch.byName from
        i <- Branch.byName into

        let merge = Branch.Merge
              { Branch.from = f
              , Branch.into = i
              }

        meta <- getCommitMeta message
        Branch.merge merge meta

      -- validate branch
      BranchValidate Validate{..} -> putStrLn $ "TODO: validate branch " <> branch

    -- print GraphQL schema
    SchemaCmd -> putStrLn Graph.schema

    -- start the server
    Server -> Server.start config

    -- print settings
    SettingsCmd -> putStrLn $ pj config

    -- manage users
    UserCmd u -> h $ case u of
      -- generate token for UAT
      UserUAT -> do
        (user, token) <- Auth.forUAT
        putStrLn $ "Authenticate for " <> userEmail user <> " with token " <> tokenUuid token

    -- display version
    VersionCmd -> case Build.info of
      Just Build.Info{..} ->
        putStrLn $ "Build " <> (show number) <> ", sha " <> (show sha)
      Nothing -> putStrLn "No build info available"

getCommitUser :: MonadIO m => m User
getCommitUser = do
  name <- gitConfig "user.name"
    "Please set a name in your global git config:\n  git.config --global user.name \"Your Name\""
  email <- gitConfig "user.email"
    "Please set an email in your global git config:\n  git.config --global user.email \"email@example.com\""
  return User
    { userName        = name
    , userEmail       = email
    , userIsReviewer  = True
    }

getCommitMeta :: MonadIO m => Text -> m CommitMeta
getCommitMeta message = CommitMeta <$> getCommitUser <*> pure message

-- TODO: if lookup fails, display a helpful message instead of the Shelly trace
gitConfig :: MonadIO m => Text -> Text -> m Text
gitConfig attribute _ = do
  out <- Sh.shelly . Sh.silently $ Sh.run "git" ["config", "--global", attribute]
  return $ T.strip out