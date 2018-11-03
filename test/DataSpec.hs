module DataSpec (spec) where

import Test.Import

spec :: Runner -> TestM TestTree
spec run = specify "Data" $ do
  todo "can run a smoke test" $ run $ return ()
