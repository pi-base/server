{-# LANGUAGE TemplateHaskell #-}
module Util.TH
  ( buildEnv
  , gitHash
  ) where

import Prelude

import Control.Monad
import Development.GitRev (gitHash)
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (Lift(..))
import System.Environment (getEnvironment)

buildEnv' :: String -> Q (Maybe String)
buildEnv' key = lookup key `liftM` runIO getEnvironment

buildEnv :: String -> Q Exp
buildEnv = (`sigE` [t| Maybe String |]) . lift <=< buildEnv'