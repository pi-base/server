{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Import
import Core

import qualified Data.Map            as M
import qualified Data.Text           as T
import           Options.Applicative
import qualified Shelly              as Sh
import           System.Log.FastLogger (flushLogStr)

import           Application         (getAppSettings, makeFoundation)
import qualified Data.Branch         as Branch
import qualified Data.Branch.Move    as Branch
import qualified Data.Branch.Merge   as Branch
import qualified Graph.Queries.Cache as Graph

data Cli = Cli
  { cmd    :: Command
  , config :: Config
  }

opts :: ParserInfo Cli
opts = info ((Cli <$> commandsP <*> configP) <**> helper) idm

data Config = Config
  { repoPath :: FilePath
  , logLevel :: LogLevel
  }

configP :: Parser Config
configP = Config
  <$> option str
        ( long "repo"
        <> metavar "REPO"
        <> help "Path to working repo"
        <> value "../data/working"
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
  = MoveCmd     Move
  | MergeCmd    Merge
  | SchemaCmd
  | ValidateCmd Validate

commandsP :: Parser Command
commandsP = subparser $ mconcat
  [ command "mv"
    ( info (MoveCmd <$> mvP <**> helper) $
      progDesc "Rename files"
    )
  , command "merge"
    ( info (MergeCmd <$> mergeP <**> helper) $
      progDesc "Merge branches"
    )
  , command "schema"
    ( info (pure SchemaCmd) $
      progDesc "Print GraphQL schema"
    )
  , command "validate"
    ( info (ValidateCmd <$> validateP <**> helper) $
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

mergeP :: Parser Merge
mergeP = Merge
  <$> argument str (metavar "FROM"    <> help "Branch to merge from")
  <*> argument str (metavar "INTO"    <> help "Branch to merge into")
  <*> argument str (metavar "MESSAGE" <> help "Full commit message text")

data Validate = Validate
  { branch :: Text
  }

validateP :: Parser Validate
validateP = Validate
  <$> argument str (metavar "BRANCH" <> help "Branch to check")

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  Cli{..} <- execParser opts

  -- TODO: run these in a more appropriate custom monad
  -- See GraphSpec for ideas

  let 
    h :: forall a. Handler a -> IO ()
    h action = do
        foundation <- configureFoundation config
        void $ unsafeHandler foundation action
        flushLogStr $ loggerSet $ appLogger foundation

  case cmd of
    -- mv files on a branch, while keeping references consistent
    MoveCmd Move{..} -> h $ do
      meta <- getCommitMeta message
      Branch.moveIds branch meta $ gatherUpdates M.empty updates
      where
        gatherUpdates :: Map Uid Uid -> [Text] -> Map Uid Uid
        gatherUpdates acc [] = acc
        gatherUpdates acc (from : to : rest) = gatherUpdates (M.insert from to $ acc) rest
        gatherUpdates _ _ = panic "Received odd number of updates"

    -- merge one branch into another, collapsing ids
    -- TODO: this should probably just always assume that `into` is the base branch
    MergeCmd Merge{..} -> h $ do
      f <- branchByName from
      i <- branchByName into

      let merge = Branch.Merge
            { Branch.from = f
            , Branch.into = i
            }

      meta <- getCommitMeta message
      Branch.merge merge meta

    -- print GraphQL schema
    SchemaCmd -> putStrLn Graph.schema

    -- validate branch
    ValidateCmd Validate{..} -> h $ putStrLn $ "TODO: validate branch " <> branch

branchByName :: Text -> Handler Branch
branchByName name = Branch.find name >>= \case
  Nothing -> panic $ "Could not find branch " <> name
  Just (Entity _ b) -> return b

configureFoundation :: Config -> IO App
configureFoundation Config{..} = do
  settings' <- getAppSettings
  -- TODO: lensify settings
  let settings = settings' 
        { appRepo = (appRepo settings')
          { rsPath = repoPath
          }
        , appLogLevel = logLevel
        }
  makeFoundation settings

panic :: Text -> a 
panic = error . T.unpack

user :: User
user = User "jamesdabbs" "jamesdabbs" "users/jamesdabbs@gmail.com" ""

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
    }

getCommitMeta :: MonadIO m => Text -> m CommitMeta
getCommitMeta message = CommitMeta <$> getCommitUser <*> pure message

-- TODO: if lookup fails, display a helpful message instead of the Shelly trace
gitConfig :: MonadIO m => Text -> Text -> m Text
gitConfig attribute _ = do
  out <- Sh.shelly . Sh.silently $ Sh.run "git" ["config", "--global", attribute]
  return $ T.strip out