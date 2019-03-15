{-# LANGUAGE TemplateHaskell #-}
module Services.Github.Types where

import Protolude

import Control.Lens (makeLenses)
import Data.Aeson   (ToJSON)

type AccessToken = Text

data Settings = Settings
  { _token         :: Text
  , _owner         :: Text
  , _repo          :: Text
  , _clientId      :: Text
  , _clientSecret  :: Text
  , _callbackUri   :: Text
  , _webhookSecret :: Text
  } deriving (Show, Eq, Generic)

makeLenses ''Settings

instance ToJSON Settings

data User = User
  { ghUserId    :: Integer
  , ghUserName  :: Maybe Text
  , ghUserEmail :: Maybe Text
  } deriving (Show, Eq, Generic)
