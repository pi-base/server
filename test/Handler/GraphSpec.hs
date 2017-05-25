module Handler.GraphSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $
  describe "Queries" $ do
    it "can get properties" $ do
      postBody GraphR "{ operationName: \"\", query: \"{ viewer { properties { uid, name } } }\" }"

      statusIs 200

      json $ \j -> do
        putStrLn $ tshow j
