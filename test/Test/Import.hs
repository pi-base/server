module Test.Import
  ( module Test.Import
  , module X
  ) where

import Core as X hiding (pass)

import Server.Class as X (App, AppM, runApp)

import Test.QuickCheck   as X (Arbitrary, property)
import Test.Tasty        as X (TestTree, testGroup)
import Test.Tasty.Hspec  as X (beforeAll, before, before_, describe, it, testSpec)

import Test.Expectations as X
import Test.Class        as X ()
import Test.Util         as X
import Test.Types        as X

import Data.Aeson.QQ             (aesonQQ)
-- import Database.Persist.Sql      (SqlPersistM, rawExecute, rawSql, unSingle)
import Language.Haskell.TH.Quote (QuasiQuoter)

import qualified Data.Branch as Branch

json :: QuasiQuoter
json = aesonQQ

testBranch :: Branch
testBranch = Branch "test" Nothing

testUser, testReviewer, steven, cody :: User
testUser     = User "Test"   "test@example.com"   False
testReviewer = User "Admin"  "admin@example.com"  True
steven       = User "Steven" "steven@pi-base.org" True
cody         = User "Cody"   "cody@pi-base.org"   False

headSha :: Sha
headSha = "4fbad4eba4b72b429eaf176df19103a06bd9ca80"

reset :: TestM ()
reset = do
  -- FIXME: wipeDB
  Branch.reset_ testBranch $ CommitSha headSha

  env <- ask
  writeIORef (env ^. http) []
  writeIORef (env ^. log ) []
  writeIORef (env ^. userRef) Nothing

-- wipeDB :: TestM ()
-- wipeDB env = runTest env $ db $ do
--   tables <- getTables
--   sqlBackend <- ask
--
--   let escapedTables = map (connEscapeName sqlBackend . DBName) tables
--       query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
--   rawExecute query []
--
-- getTables :: SqlPersistM [Text]
-- getTables = do
--   let query = mconcat
--         [ "SELECT table_name"
--         , "FROM information_schema.tables"
--         , "WHERE table_schema = 'public';"
--         ]
--   tables <- rawSql query []
--
--   return $ map unSingle tables