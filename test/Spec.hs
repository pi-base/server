module Main
  ( main
  ) where

import ClassyPrelude
import Test.Tasty (TestTree, defaultMain, testGroup)
import TestImport (TestApp, App, buildApp)
import Util       (memoized)

import qualified Data.ParseSpec
import qualified Handler.CommonSpec
import qualified Handler.GraphSpec
import qualified Handler.HomeSpec
import qualified GraphSpec
import qualified PageSpec

main :: IO ()
main = do
  appRef <- newIORef Nothing
  t <- specs $ memoized appRef $ buildApp
  defaultMain . testGroup "Pi-Base" $ t

specs :: IO (TestApp App) -> IO [TestTree]
specs app = sequence
  [ Data.ParseSpec.spec app
  , GraphSpec.spec app
  , PageSpec.spec
  , Handler.CommonSpec.spec app
  , Handler.GraphSpec.spec app
  , Handler.HomeSpec.spec app
  ]
