module Types
  ( module Types
  ) where

import Protolude

import Data.Aeson (FromJSON, ToJSON)

newtype Version = Version { unVersion :: Text }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

data OAuth2 = OAuth2
  { clientId     :: Text
  , clientSecret :: Text
  , callbackUri  :: Text
  } deriving (Show, Eq, Generic)
