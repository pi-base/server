import qualified Client.Reason       as Reason
import           Core                hiding (interpret, option)
import qualified Data.Branch         as Branch
import           Options.Applicative
import qualified Interpreter
import qualified Persist.Branches    as Branches
import qualified Persist.DB          as DB
import qualified Server
import qualified Server.Status       as Status

main :: IO ()
main = do
  defaults      <- defaultConfig
  (config, cmd) <- execParser $ info (parser defaults <**> helper) fullDesc
  print config
  run config cmd

parser :: Server.Config -> Parser (Server.Config, Command)
parser defaults = (,)
  <$> configP defaults
  <*> commandP

defaultConfig :: IO Server.Config
defaultConfig = Server.Config
  <$> pure 31415
  <*> pure False
  <*> ( Interpreter.io
      <$> pure (Branch "master")
      <*> Status.build
      )

configP :: Server.Config -> Parser Server.Config
configP c = Server.Config
  <$> option auto
      ( long "port"
      <> short 'p'
      <> metavar "PORT"
      <> value 3000
      )
  <*> switch
      ( long "dev-logging"
      <> help "Enable rich request logging"
      )
  <*> interpreterP (Server.interpreter c)

interpreterP :: Interpreter.Config -> Parser Interpreter.Config
interpreterP c = Interpreter.Config
  <$> masterP (c ^. Interpreter.master)
  <*> storeP  (c ^. Interpreter.store)
  <*> httpP   (c ^. Interpreter.http)
  <*> authP   (c ^. Interpreter.auth)
  <*> pure    (c ^. Interpreter.status)

masterP :: Branch -> Parser Branch
masterP b = Branch
  <$> option auto
      ( long "base-branch"
      <> metavar "BRANCH"
      <> value (Branch.name b)
      )

storeP :: Interpreter.StoreConfig -> Parser Interpreter.StoreConfig
storeP = pure

httpP :: Interpreter.HttpConfig -> Parser Interpreter.HttpConfig
httpP = pure

authP :: Interpreter.AuthConfig -> Parser Interpreter.AuthConfig
authP = pure

data Command
  = Branches BranchesCommand
  | Codegen FilePath
  | Migrate
  | Server
  | Users UsersCommand

commandP :: Parser Command
commandP = subparser
  ( command "branches"
    ( info
      (fmap Branches branchesP <**> helper)
      (progDesc "Manage repository branches")
    )
  <> command "codegen"
    ( info
      (codegenP <**> helper)
      (progDesc "Generate Reason client code")
    )
  <> command "migrate"
    ( info
      (pure Migrate <**> helper)
      (progDesc "Run migrations")
    )
  <> command "server"
    ( info
      (pure Server <**> helper)
      (progDesc "Start the server")
    )
  <> command "users"
    ( info
      (fmap Users usersP <**> helper)
      (progDesc "Manage users")
    )
  )

codegenP :: Parser Command
codegenP = Codegen
  <$> option auto
      ( long "path"
      <> short 'p'
      <> metavar "PATH"
      <> help "Path to write output files"
      <> value "build/Api.re"
      )

data BranchesCommand
  = ListBranches

branchesP :: Parser BranchesCommand
branchesP = subparser
  ( command "list"
    ( info
      (pure ListBranches <**> helper)
      (progDesc "List branches")
    )
  )

data UsersCommand
  = CreateUser User
  | ListUsers

usersP :: Parser UsersCommand
usersP = subparser
  ( command "create"
    ( info
      (fmap CreateUser user <**> helper)
      (progDesc "Create users")
    )
  <> command "list"
    ( info
      (pure ListUsers <**> helper)
      (progDesc "List users")
    )
  )
  where
    user = User
      <$> option auto
          ( long "name"
          <> metavar "NAME"
          )
      <*> option auto
          ( long "email"
          <> metavar "EMAIL"
          )
      <*> pure False

interpret cfg action = do
  env <- Interpreter.boot $ Server.interpreter cfg
  Interpreter.serve $ Interpreter.run env $ action

run :: Server.Config -> Command -> IO ()
run cfg (Branches ListBranches) = interpret cfg $ do
  branches <- Branches.scan
  putStrLn (show (length branches) <> " branches" :: Text)
  forM_ branches print

run _ (Codegen path) = do
  putStrLn $ "Writing client code to " <> path
  writeFile path Reason.code

run cfg Migrate = interpret cfg $ do
  DB.migrate

run config Server = do
  putStrLn $ "Listening on port " <> (show $ Server.port config :: Text)
  -- when (Server.storage config == Server.InMemory) $
  --   putStrLn ("Using in-memory storage" :: Text)
  -- FIXME
  Server.start config

run _ (Users (CreateUser u)) = print u
run _ (Users ListUsers) = print "List users"
