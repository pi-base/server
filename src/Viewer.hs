{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Viewer
  ( Viewer(..)
  , empty
  ) where

import Core    hiding (empty)
import Formula hiding ((.=))

import           Data.Aeson
import qualified Data.Map   as M

empty :: Viewer
empty = Viewer
  { viewerSpaces = []
  , viewerProperties = []
  , viewerTraits = []
  , viewerTheorems = []
  , viewerProofs = M.empty
  , viewerVersion = Version ""
  }
