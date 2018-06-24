{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Debug where

import ClassyPrelude

import           Conduit
import           Data.Aeson               (ToJSON)
import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as TL

pj :: ToJSON a => a -> Text
pj = TL.toStrict . decodeUtf8 . encodePretty

traceC :: (Monad m, Show d) => (o -> d) -> ConduitM o o m ()
traceC f = mapC $ \x -> trace (show $ f x) x

traceJ :: (ToJSON a, Monad m) => a -> m ()
traceJ = traceM . T.unpack . pj
