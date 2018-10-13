{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Import hiding (logLevel)
import Core

import qualified Data.Map              as M
import qualified Data.Text             as T
import           Options.Applicative
import qualified Shelly                as Sh
import           System.Directory      (setCurrentDirectory)
import           System.Environment    (getExecutablePath, lookupEnv)
import           System.FilePath       (takeDirectory)
import           System.Log.FastLogger (flushLogStr)

import           Application         (appMain, getAppSettings, makeFoundation)
import qualified Data.Branch         as Branch
import qualified Data.Branch.Move    as Branch
import qualified Data.Branch.Merge   as Branch
import           Data.Helpers        (findOrCreate)
import qualified Graph.Queries.Cache as Graph
import qualified Settings            as S

data Cli = Cli
  { cmd    :: Command
  , config :: Config
  }

opts :: AppSettings -> ParserInfo Cli
opts s = info ((Cli <$> commandsP s <*> configP s) <**> helper) idm

settingsP :: Parser FilePath
settingsP = option str
  ( long "config"
  <> metavar "CONFIG"
  <> help "Path to settings config file"
  <> value "config/settings.yml"
  )

settingsInfo :: ParserInfo FilePath
settingsInfo = info (settingsP <**> helper) idm

data Config = Config
  { repoPath :: FilePath
  , logLevel :: LogLevel
  }

configP :: AppSettings -> Parser Config
configP s = Config
  <$> option str
        ( long "repo"
        <> metavar "REPO"
        <> help "Path to working repo"
        <> value (rsPath $ appRepo s)
        )
  <*> option (eitherReader parseLogLevel)
        ( long "verbosity"
        <> short 'v'
        <> metavar "LOG_LEVEL"
        <> help "Logger verbosity level"
        <> value LevelInfo
        )

parseLogLevel :: String -> Either String LogLevel
parseLogLevel "error" = Right LevelError
parseLogLevel "warn"  = Right LevelWarn
parseLogLevel "info"  = Right LevelInfo
parseLogLevel "debug" = Right LevelDebug
parseLogLevel other = Left $ "Unknown log level: " ++ other

data Command
  = BranchCmd BranchCommand
  | SchemaCmd
  | Server
  | VersionCmd

commandsP :: AppSettings -> Parser Command
commandsP s = subparser $ mconcat
  [ command "branch"
    ( info (BranchCmd <$> branchCommandP (appRepo s) <**> helper) $
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

branchCommandP :: RepoSettings -> Parser BranchCommand
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

mergeP :: RepoSettings -> Parser Merge
mergeP RepoSettings{..} = Merge
  <$> option str
      ( long "from"
      <> help "Branch to merge from"
      )
  <*> option str
      ( long "into"
      <> help "Branch to merge into"
      <> showDefault
      <> value rsDefaultBranch
      )
  <*> option str
      ( long "message"
      <> short 'm'
      <> help "Full commit message text"
      )

data Validate = Validate
  { branch :: Text
  }

validateP :: Parser Validate
validateP = Validate
  <$> argument str (metavar "BRANCH" <> help "Branch to check")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  -- We expect that when deployed, the binary will be co-located
  -- with its config files
  lookupEnv "AUTO_CD" >>= \case
    Just "false" -> return ()
    _ -> do
      workDir <- takeDirectory <$> getExecutablePath
      setCurrentDirectory workDir

  settingsPath <- execParser settingsInfo
  settings' <- getAppSettings
  Cli{..} <- execParser $ opts settings'
  let settings = overrideSettings settings' config

  -- TODO: run these in a more appropriate custom monad
  -- See GraphSpec for ideas

  let
    h :: forall a. Handler a -> IO ()
    h m = do
        foundation <- makeFoundation settings
        void $ unsafeHandler foundation m
        flushLogStr $ loggerSet $ appLogger foundation

  case cmd of
    BranchCmd b -> h $ case b of
      BranchList -> do
        void $ Branch.claimUserBranches
        branches <- Branch.all

        lines <- forM branches $ \b -> do
          sha <- Branch.headSha b
          return $ branchName b <> " @ " <> sha

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
        f <- branchByName from
        i <- branchByName into

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
    Server -> appMain

    -- display version
    VersionCmd -> putStrLn $ "Build " <> (tshow $ ciBuild ciSettings) <> ", sha " <> (tshow $ ciSha ciSettings)

branchByName :: Text -> Handler Branch
branchByName name = do
  (Entity _ branch) <- findOrCreate (UniqueBranchName . branchName) $ Branch name Nothing
  return branch

overrideSettings :: AppSettings -> Config -> AppSettings
overrideSettings settings' Config{..} = settings'
  { appRepo = (appRepo settings')
    { rsPath = repoPath
    }
  , appLogLevel = logLevel
  }

panic :: Text -> a
panic = error . T.unpack

user :: User
user = User "jamesdabbs" "jamesdabbs" "users/jamesdabbs@gmail.com" "" True

getCommitUser :: MonadIO m => m User
getCommitUser = do
  name <- gitConfig "user.name"
    "Please set a name in your global git config:\n  git.config --global user.name \"Your Name\""
  email <- gitConfig "user.email"
    "Please set an email in your global git config:\n  git.config --global user.email \"email@example.com\""
  return User
    { userName        = name
    , userEmail       = email
    -- TODO: separate User / Identity models; we should be able to generate a User
    --   without having Github OAuth credentials
    , userIdent       = "cli:" <> name
    , userGithubToken = ""
    , userIsReviewer  = True
    }

getCommitMeta :: MonadIO m => Text -> m CommitMeta
getCommitMeta message = CommitMeta <$> getCommitUser <*> pure message

-- TODO: if lookup fails, display a helpful message instead of the Shelly trace
gitConfig :: MonadIO m => Text -> Text -> m Text
gitConfig attribute _ = do
  out <- Sh.shelly . Sh.silently $ Sh.run "git" ["config", "--global", attribute]
  return $ T.strip out