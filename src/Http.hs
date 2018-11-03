module Http
  ( module X
  , Http(..)
  , jsonBody
  ) where

import Protolude

import           Control.Lens
import           Data.Aeson           hiding (Options)
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as LBS
import           Network.Wreq
import           Network.Wreq.Types

import qualified Network.Wreq as X (defaults, header, param)

class MonadIO m => Http m where
  get  :: Options -> Text -> m Value
  post :: Postable a => Options -> Text -> a -> m Value

jsonBody :: Response LBS.ByteString -> Value
jsonBody res = fromMaybe Null $ res ^? responseBody . _Value
