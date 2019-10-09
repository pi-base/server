module Data.MemoSpec (spec) where

import Test.Import

-- import Data.Memo as Memo

spec :: IO TestTree
spec = testSpec "Data.Memo" $ parallel $ do
  -- TODO: Persist.Memo
  return ()
  -- describe "fetch" $ do
  --   it "bumps the count when the key changes" $ do
  --     memo <- init

  --     count <- newIORef (0 :: Integer)
  --     let update = atomicModifyIORef' count $ \n -> (n + 1, n + 1)

  --     fetch memo 'a' update `shouldReturn` 1
  --     fetch memo 'a' update `shouldReturn` 1
  --     fetch memo 'a' update `shouldReturn` 1
  --     fetch memo 'b' update `shouldReturn` 2
  --     fetch memo 'a' update `shouldReturn` 3
  --     fetch memo 'z' update `shouldReturn` 4
  --     fetch memo 'z' update `shouldReturn` 4
