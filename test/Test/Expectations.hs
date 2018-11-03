module Test.Expectations where

import Core hiding (pass)

import           Test.Tasty           (TestName, TestTree)
import           Test.Tasty.Hspec     (testSpec)
import           Test.Hspec           hiding (shouldBe)
import qualified Test.Hspec           as Hspec
import           Test.Hspec.Core.Spec (SpecM)

import           Data.List           (isInfixOf)
import qualified Data.Text           as T
import           System.Console.ANSI (Color(..))
import           System.Environment  (lookupEnv)

specify :: MonadIO m => TestName -> Spec -> m TestTree
specify name = liftIO . testSpec name

ci :: (Arg a ~ (), Example a)
     => String
     -> a
     -> SpecM (Arg a) ()
ci title action = do
  isCI <- runIO $ lookupEnv "CI"
  if (isCI == Just "true")
    then it title action
    else it title $ do
           putStr $ (colorize Blue "CI") <> " - "
           pass

-- xit / pending currently count as a failure on CI
todo :: String -> t -> SpecWith ()
todo msg _ = it ((T.unpack $ "⏳ ") <> msg) pass

pass :: Expectation
pass = shouldBe () ()

pending :: MonadIO m => m ()
pending = liftIO Hspec.pending

pendingWith :: MonadIO m => String -> m ()
pendingWith = liftIO . Hspec.pendingWith

colorize :: Color -> Text -> Text
colorize _color text = text -- FIXME

shouldBe :: (Eq a, Show a, MonadIO m) => a -> a -> m ()
shouldBe a b = liftIO $ Hspec.shouldBe a b

shouldInclude :: (Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldInclude haystack needle = liftIO $ (needle `isInfixOf` haystack) `shouldBe` True

shouldSatisfy :: (Show a, MonadIO m) => a -> (a -> Bool) -> m ()
shouldSatisfy a cond = liftIO $ Hspec.shouldSatisfy a cond

shouldStartWith :: (Eq a, MonadIO m) => [a] -> [a] -> m ()
shouldStartWith haystack needle = liftIO $ (needle `isPrefixOf` haystack) `shouldBe` True

shouldThrow :: (MonadUnliftIO m, Exception e) => m a -> Selector e -> m ()
shouldThrow action selector = do
  unlift <- askUnliftIO
  liftIO $ Hspec.shouldThrow (unliftIO unlift action) selector

infix 1 `shouldBe`
infix 1 `shouldInclude`
infix 1 `shouldStartWith`
infix 1 `shouldThrow`
