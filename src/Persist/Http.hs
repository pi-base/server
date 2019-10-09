{-# LANGUAGE TemplateHaskell #-}
module Persist.Http
  ( Http
  , Persist.Http.get
  , jsonBody
  , post
  , runList
  , runWreq
  -- Re-exports
  , Wreq.defaults
  , Wreq.header
  , Wreq.param
  ) where

import Core hiding (toList)

import           Control.Lens
import           Data.Aeson           hiding (Options)
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text            as Text
import qualified Network.Wreq         as Wreq
import qualified Network.Wreq.Types   as Wreq
import           Polysemy             (embed)
import           Polysemy.State       as S (State, evalState, get, put)

data Http m a where
  Get  :: Wreq.Options -> Text -> Http m Value
  Post :: Wreq.Postable a => Wreq.Options -> Text -> a -> Http m Value

makeSem ''Http

jsonBody :: Wreq.Response LBS.ByteString -> Value
jsonBody res = fromMaybe Null $ res ^? Wreq.responseBody . _Value

runWreq :: Member (Embed IO) r => Sem (Http ': r) a -> Sem r a
runWreq = interpret \case
  Get  opts path      -> embed $ jsonBody <$> Wreq.getWith  opts (Text.unpack path)
  Post opts path body -> embed $ jsonBody <$> Wreq.postWith opts (Text.unpack path) body

toList :: Sem (Http ': r) a -> Sem (State [Value] ': r) a
toList = reinterpret \case
  Get    _ _ -> next
  Post _ _ _ -> next
  where
    next :: Sem (State [Value] ': r) Value
    next = S.get >>= \case
      (a : as) -> put as >> return a
      _ -> return Null

runList :: [Value] -> Sem (Http ': r) a -> Sem r a
runList vs action = toList action & evalState vs
