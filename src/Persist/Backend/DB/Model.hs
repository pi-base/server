{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Persist.Backend.DB.Model where

import Protolude

import Database.Persist.Quasi
import Database.Persist.TH
import Data.Access            (Access)
import Data.Time              (UTCTime)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

instance Ord Branch where
  compare = compare `on` branchName

instance Ord User where
  compare = compare `on` userEmail
