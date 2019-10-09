module Types
  ( module Types
  , module X
  ) where

import Protolude

import Database.Persist         as X (Entity(..))
import Persist.Backend.DB.Model as X

import Data.Aeson (FromJSON, ToJSON)

newtype Version = Version { unVersion :: Text }
  deriving (Generic, Eq, Show, ToJSON, FromJSON)

data OAuth2 = OAuth2
  { clientId     :: Text
  , clientSecret :: Text
  , callbackUri  :: Text
  } deriving (Show, Eq, Generic)
