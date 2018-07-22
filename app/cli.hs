{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll #-}
import Import
import Core

import qualified Data.Map            as M
import qualified Data.Text           as T
import           Options.Applicative
import           System.Log.FastLogger (flushLogStr)

import           Application         (getAppSettings, makeFoundation)
import qualified Data.Branch         as Branch
import qualified Data.Branch.Move    as Branch
import qualified Data.Branch.Merge   as Branch
import qualified Graph.Introspection as Graph

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
  { branch :: BranchName
  , from   :: Uid
  , to     :: Uid
  }

mvP :: Parser Move
mvP = Move
  <$> argument str (metavar "BRANCH" <> help "Branch to update")
  <*> argument str (metavar "FROM" <> help "Current path")
  <*> argument str (metavar "TO" <> help "Path to move to")

data Merge = Merge
  { from :: Text
  , into :: Text
  }

mergeP :: Parser Merge
mergeP = Merge
  <$> argument str (metavar "FROM" <> help "Branch to merge from")
  <*> argument str (metavar "INTO" <> help "Branch to merge into")

data Validate = Validate
  { branch :: Text
  }

validateP :: Parser Validate
validateP = Validate
  <$> argument str (metavar "BRANCH" <> help "Branch to check")

-- Branch.merge merge meta

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  Cli{..} <- execParser opts

  let 
    h :: forall a. Handler a -> IO ()
    h action = do
        foundation <- configureFoundation config
        void $ unsafeHandler foundation action
        flushLogStr $ loggerSet $ appLogger foundation

  case cmd of
    -- mv branch from to
    MoveCmd Move{..} -> h $ do
      Branch.move branch $ M.fromList [(from, to)]

    -- merge from into
    MergeCmd Merge{..} -> h $ do
      f <- branchByName from
      i <- branchByName into

      -- TODO:
      let user = User "jamesdabbs" "jamesdabbs" "users/jamesdabbs@gmail.com" ""
          meta = CommitMeta user "Test Merge"
          merge = Branch.Merge
            { Branch.from = f
            , Branch.into = i
            }

      Branch.merge merge meta

    -- schema
    SchemaCmd -> void $ case Graph.renderSchema of
      Left err -> do
        putStrLn "Error when rendering schema"
        panic $ tshow err
      Right doc -> do
        putStrLn doc
        putStrLn "# N.B. schema printing support does not currently include input types"

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