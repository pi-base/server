{-# LANGUAGE Rank2Types #-}
module Services.HTTP
  ( HTTP(..)
  , module X
  , wreq
  ) where

import Protolude

import Network.Wreq as X (Options, defaults, header, param, responseBody)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as T
import qualified Network.Wreq         as W
import qualified Network.Wreq.Types   as W

data HTTP = HTTP
  { get  :: forall m. MonadIO m => Text -> W.Options -> m (W.Response LBS.ByteString)
  , post :: forall m a. (MonadIO m, W.Postable a) => Text -> W.Options -> a -> m (W.Response LBS.ByteString)
  }

wreq :: HTTP
wreq = HTTP
  { get = \path opts -> liftIO $ W.getWith opts (T.unpack path)
  , post = \path opts body -> liftIO $ W.postWith opts (T.unpack path) body
  }

