{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Data.Id
  ( Identifiable(..)
  , assignId
  , toInt
  , fromInt
  , pending
  , succ
  ) where

import Protolude hiding (succ)
import Types

import           Data.Attoparsec.Text as A
import           Data.Text
import qualified Data.UUID            as UUID
import qualified Data.UUID.V4         as UUID

class Identifiable a where
  prefix :: Char
  -- TODO: lensify
  getId  :: a -> Id a
  setId  :: a -> Id a -> a

instance Identifiable Space where
  prefix     = 'S'
  getId      = spaceId
  setId s id = s { spaceId = id }
instance Identifiable Property where
  prefix     = 'P'
  getId      = propertyId
  setId p id = p { propertyId = id }
instance Identifiable (Theorem PropertyId) where
  prefix     = 'I'
  getId      = theoremId
  setId t id = t { theoremId = id }

toInt :: forall a. Identifiable a => Id a -> Maybe Int
toInt (Id txt) = either (const Nothing) Just $ flip parseOnly txt $ do
  _ <- char (prefix @a)
  _ <- A.takeWhile $ \c -> c == '0'
  decimal

fromInt :: forall a. Identifiable a => Int -> Id a
fromInt i = Id $ singleton (prefix @a) <> num
  where num = justifyRight 6 '0' $ pack $ show i

succ :: forall a. Identifiable a => Id a -> Id a
succ id = case toInt @a id of
  Just i  -> fromInt @a $ i + 1
  Nothing -> fromInt @a 1

pending :: forall a. Identifiable a => Id a
pending = Id ""

assignId :: forall a m. (Identifiable a, MonadIO m) => a -> m a
assignId a = if getId a == pending
  then do
    uuid <- liftIO UUID.nextRandom
    let uid = (toLower . singleton $ prefix @a) <> UUID.toText uuid
    return $ setId a $ Id uid
  else return a
