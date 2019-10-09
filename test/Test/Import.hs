module Test.Import
  ( module Test.Import
  , module X
  ) where

import Core as X hiding (pass)

import Polysemy          as X (runM)
import Polysemy.Fail     as X (Fail)
import Test.Expect       as X
import Test.Hspec        as X (parallel)
import Test.QuickCheck   as X (Arbitrary)
import Test.Tasty        as X (TestTree, testGroup)
import Test.Tasty.Hspec  as X (Spec, SpecWith, beforeAll, before, before_, describe, it, runIO, testSpec)
import Text.RawString.QQ as X (r)

import Test.Class ()

import           Control.Monad.Reader        (ask)
import           Data.Aeson.QQ               (aesonQQ)
import qualified Data.Text                   as Text
import           Database.Persist.Postgresql (DBName(DBName), connEscapeName, rawExecute, rawSql, runMigration, unSingle)
import           Language.Haskell.TH.Quote   (QuasiQuoter)
import qualified Persist.DB                  as DB
import qualified Persist.Backend.Git         as Git
import qualified Persist.Github              as Github
import           Polysemy.Fail               (failToEmbed)
import qualified Server.Status               as Status
import           System.IO.Temp              (withSystemTempDirectory)
import           System.IO.Unsafe            (unsafePerformIO)

json :: QuasiQuoter
json = aesonQQ

dbConfig :: DB.Config
dbConfig = DB.Config
  "postgresql://localhost/pi_base_test"
  5

githubConfig :: Github.Config
githubConfig = Github.Config
  { credentials = OAuth2
    { clientId     = "test"
    , clientSecret = "test"
    , callbackUri  = "http://localhost:8000"
    }
  , token       = "test"
  , repo        = "data.test"
  , owner       = "pi-base"
  }

runDB :: DB.Env -> Sem '[DB.DB, Embed IO] a -> IO a
runDB env = runM . DB.runIO env

setupDB :: IO DB.Env
setupDB = do
  env <- DB.initialize dbConfig
  runDB env $ DB.migrate
  return env

resetDB :: DB.Env -> IO ()
resetDB env = runDB env $ DB.query $ do
  let tablesQuery = Text.intercalate " "
        [ "SELECT table_name"
        , "FROM information_schema.tables"
        , "WHERE table_schema = 'public';"
        ]
  tables  <- map unSingle <$> rawSql tablesQuery []

  backend <- ask

  let
    escapedTables = map (connEscapeName backend . DBName) tables
    query = "TRUNCATE TABLE " <> Text.intercalate ", " escapedTables

  rawExecute query []

testMaster :: Branch
testMaster = Branch "master" -- TODO: why do we get an error if this is `"test"` instead of `"master"`?

withTempRepo :: (Git.Env -> IO a) -> IO a
withTempRepo handler =
  withSystemTempDirectory "pibase.test" $ \dir -> do
    let config = Git.Config (dir <> "/repo.git")
    Git.initialize config >>= handler

{-# NOINLINE now #-}
now :: UTCTime
now = unsafePerformIO getCurrentTime

testStatus :: Status.Status
testStatus = Status.mkStatus now

runTest :: Sem '[Expect, Fail, Embed IO] a -> IO a
runTest action = action
  & check
  & failToEmbed
  & runM
