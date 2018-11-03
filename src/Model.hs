{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Model where

import Protolude

import Database.Persist.Quasi
import Database.Persist.TH
import Data.Time              (UTCTime)
import Model.Fields           ()
import Types.Base

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")
