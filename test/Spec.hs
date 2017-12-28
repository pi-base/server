module Main
  ( main
  ) where

import Prelude
import Test.Tasty (defaultMain, testGroup)

import qualified GraphSpec

main :: IO ()
main = do
  t <- sequence specs
  defaultMain . testGroup "Pi-Base" $ t
  where
    specs =
      [ GraphSpec.spec
      ]
