{-# LANGUAGE PackageImports #-}
{-# LANGUAGE QuasiQuotes #-}
module TestImport
    ( module TestImport
    , module X
    ) where

import Application           (makeFoundation)
#if MIN_VERSION_classy_prelude(1, 0, 0)
import ClassyPrelude         as X hiding (delete, deleteBy, Handler)
#else
import ClassyPrelude         as X hiding (delete, deleteBy)
#endif
import Control.Monad.Trans.State as X (StateT)
import Database.Persist      as X hiding (get)
import Foundation            as X
import Logging               as X
import Model                 as X
import Test.Hspec            as X
import Test.Tasty            as X (TestTree)
import Test.Tasty.Hspec      as X (testSpec)
import UnliftIO.Exception    as X hiding (Handler)
import Yesod.Default.Config2 (useEnv, loadYamlSettings)
import Yesod.Auth            as X
import Yesod.Test            as X

import           Data.Aeson            (Value(..), decode)
import qualified Data.ByteString.Lazy  as LBS
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import qualified Data.HashMap.Strict   as HM
import           Network.Wai.Test      (SResponse(..))
import           System.Environment    (lookupEnv)
import qualified Test.HUnit            as H
import           Test.Hspec.Core.Spec  (SpecM)
import           Text.Shakespeare.Text (st)

import "pi-base-server" Debug as X

import Data.Store (initializeDownstream)
import Settings   (appRepo)

-- Wiping the database

import Database.Persist.Sql  (SqlPersistM, runSqlPersistMPool, rawExecute, rawSql, unSingle, connEscapeName)

runDB :: SqlPersistM a -> YesodExample App a
runDB query = do
    app <- getTestYesod
    liftIO $ runDBWithApp app query

runDBWithApp :: App -> SqlPersistM a -> IO a
runDBWithApp app query = runSqlPersistMPool query (appConnPool app)

buildApp :: IO (TestApp App)
buildApp = do
  settings <- loadYamlSettings
      ["config/test-settings.yml", "config/settings.yml"]
      []
      useEnv
  foundation <- makeFoundation settings
  wipeDB foundation
  -- This assumes that we're testing with downstream pointed to a local dir
  unsafeHandler foundation $ initializeDownstream $ appRepo settings
  return (foundation, id)

withApp :: SpecWith (TestApp App) -> Spec
withApp = before buildApp

-- This function will truncate all of the tables in your database.
-- 'withApp' calls it before each test, creating a clean environment for each
-- spec to run in.
wipeDB :: App -> IO ()
wipeDB app = runDBWithApp app $ do
    tables <- getTables
    sqlBackend <- ask

    let escapedTables = map (connEscapeName sqlBackend . DBName) tables
        query = "TRUNCATE TABLE " ++ intercalate ", " escapedTables
    rawExecute query []

getTables :: SqlPersistM [Text]
getTables = do
    tables <- rawSql [st|
        SELECT table_name
        FROM information_schema.tables
        WHERE table_schema = 'public';
    |] []

    return $ map unSingle tables

-- | Authenticate as a user. This relies on the `auth-dummy-login: true` flag
-- being set in test-settings.yaml, which enables dummy authentication in
-- Foundation.hs
authenticateAs :: Entity User -> YesodExample App ()
authenticateAs (Entity _ u) = do
    request $ do
        setMethod "POST"
        addPostParam "ident" $ userIdent u
        setUrl $ AuthR $ PluginR "dummy" []

-- | Create a user.
createUser :: Text -> Text -> YesodExample App Text
createUser ident token = do
  userId <- runDB $ insert User
      { userIdent       = ident
      , userName        = ident
      , userEmail       = ident
      , userGithubToken = ident
      }

  now <- liftIO getCurrentTime

  _ <- runDB $ insert Token
    { tokenUserId = userId
    , tokenIssuedAt = now
    , tokenExpiredAt = Nothing
    , tokenUuid = token
    }

  return ident

json :: (Value -> YesodExample App ()) -> YesodExample App ()
json action = withResponse $ \SResponse { simpleBody = body } ->
  case decode body of
    Nothing     -> liftIO $ H.assertBool "Can't parse response as JSON" False
    Just parsed -> action parsed

getResponseBody :: YesodExample site LBS.ByteString
getResponseBody = getResponse >>= return . maybe "" simpleBody

showResponse :: YesodExample a ()
showResponse = withResponse $ \SResponse { simpleBody = body } -> liftIO . putStrLn . TL.toStrict $ decodeUtf8 body

shouldHaveKey :: Value -> Text -> YesodExample App ()
shouldHaveKey (Object _map) key = liftIO $ H.assertBool msg (HM.member key _map)
  where msg = "Value does not contain key: " ++ T.unpack key
shouldHaveKey _ _ = liftIO $ H.assertBool "Value is not an object" False

slow :: (Arg a ~ (), Example a) 
     => String
     -> a
     -> SpecM (Arg a) ()
slow title action = do
  ci <- runIO $ lookupEnv "CI"
  if (ci == Just "true")
    then it title action
    else it title $ do
           putStr $ (colorize Blue "CI") <> " - "
           pass
           
-- xit / pending currently count as a failure on CI
todo :: String -> t -> SpecWith ()
todo msg _ = it msg $ do
  putStr $ (colorize Yellow "TODO") <> " - "
  pass

pass :: Expectation
pass = shouldBe () ()

colorize :: Color -> Text -> Text
colorize color text = decodeUtf8 $ mconcat $ ansiColor color $ encodeUtf8 text

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

fromRight :: Either a b -> b
fromRight (Right b) = b
fromRight _ = error "got left"

shouldInclude :: Eq a => [a] -> [a] -> Expectation
shouldInclude haystack needle = (needle `isInfixOf` haystack) `shouldBe` True