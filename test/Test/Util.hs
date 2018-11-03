module Test.Util
  ( login
  , logout
  , stub
  ) where

import Server.Import

import Control.Lens (view)

import Test.Class ()
import Test.Types

import qualified Data.Branch as Branch
import           Util        (findOrCreate)

login :: User -> TestM ()
login u = do
  eu  <- findOrCreate (UniqueUser . userEmail) u
  _   <- Branch.ensureUserBranch eu
  ref <- view userRef
  writeIORef ref $ Just eu

logout :: TestM ()
logout = do
  ref <- view userRef
  writeIORef ref Nothing

stub :: Text -> Value -> TestM ()
stub _path resp = do
  net <- view http
  -- TODO: register with path (and method) matchers
  modifyIORef' net $ \resps -> resps ++ [resp]

