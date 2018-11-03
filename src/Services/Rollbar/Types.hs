{-# LANGUAGE TemplateHaskell #-}
module Services.Rollbar.Types where

import Protolude

import Control.Lens (makeLenses)
import Data.Aeson   (ToJSON)

data Settings = Settings
  { _token       :: Maybe Text
  , _environment :: Text
  , _build       :: Text
  } deriving (Show, Eq, Generic)

makeLenses ''Settings

instance ToJSON Settings
