module Main
  ( main
  ) where

import Prelude
import Test.Tasty (TestTree, defaultMain, testGroup)

import qualified Handler.CommonSpec
import qualified Handler.GraphSpec
import qualified Handler.HomeSpec
import qualified GraphSpec
import qualified PageSpec

main :: IO ()
main = do
  t <- specs
  defaultMain . testGroup "Pi-Base" $ t
  where
    specs :: IO [TestTree]
    specs = sequence
      [ Handler.CommonSpec.spec
      , Handler.GraphSpec.spec
      , Handler.HomeSpec.spec
      , GraphSpec.spec
      , PageSpec.spec
      ]
