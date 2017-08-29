module Data.SpaceSpec (spec) where

import TestImport

import Core
import qualified Data.Space as Space

spec :: Spec
spec = hspec $ do
  describe "Updating a space" $ do
    it "can create a new space" $ do
      let space = Space "id" "name" "desc" "slug" Nothing
          user  = User "ident" "name" "email" "token"
          msg   = CommitMeta user "test create space"
          ref   = Ref "test"
      (version, space') <- Space.put ref msg space
      print (version, space')
