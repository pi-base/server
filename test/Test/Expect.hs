{-# LANGUAGE TemplateHaskell #-}
module Test.Expect
  ( Expect
  , assert
  , check
  , expectLeft
  , expectRight
  , (<==)
  , (===)
  , shouldBe
  ) where

import Import

import qualified Data.Text  as Text
import           Polysemy   (embed)
import qualified Test.Hspec as H

data Expect m a where
  Fail :: Text -> Expect m b

makeSem ''Expect

check :: Member (Embed IO) r => Sem (Expect ': r) a -> Sem r a
check = interpret $ \case
  Fail msg -> embed $ do
    H.expectationFailure $ Text.unpack msg
    undefined -- TODO: get this to typecheck properly

assert :: Member Expect r => Bool -> Text -> Sem r ()
assert value message = unless value $ fail message

(<==) :: (Member Expect r, Show a, Eq a) => a -> Sem r a -> Sem r ()
(<==) expected action = do
  actual <- action
  actual === expected

infix 0 <==

(===) :: (Member Expect r, Show a, Eq a) => a -> a -> Sem r ()
(===) actual expected = assert (actual == expected) $
  "got " <> show actual <> ", but expected " <> show expected

infix 0 ===

expectRight :: (Member Expect r, Show e) => Either e a -> Sem r a
expectRight (Right a) = return a
expectRight (Left e) = fail $ "unexpected left " <> show e

expectLeft :: (Member Expect r, Show a) => Either e a -> Sem r e
expectLeft (Left e) = return e
expectLeft (Right a) = fail $ "unexpected right " <> show a

shouldBe :: (Show a, Eq a) => a -> a -> IO ()
shouldBe = H.shouldBe

infix 0 `shouldBe`