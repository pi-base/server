{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Debug where

import Protolude

import           Conduit
import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy     as LBS
import qualified Data.Text                as T
import qualified Prelude                  (error)

pj :: ToJSON a => a -> Text
pj = decodeUtf8 . LBS.toStrict . encodePretty

traceC :: (Monad m, Show d) => (o -> d) -> ConduitM o o m ()
traceC f = mapC $ \x -> trace (show (f x) :: Text) x

traceJ :: (ToJSON a, Monad m) => a -> m ()
traceJ = traceM . pj

traceS :: (Show a, Monad m) => a -> m ()
traceS a = traceM (show a :: Text)

error :: HasCallStack => Text -> a
error = Prelude.error . T.unpack
