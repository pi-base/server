{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Model.Fields where

import Database.Persist.TH
import Types.Base

derivePersistField "BranchAccess"
