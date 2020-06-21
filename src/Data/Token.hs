{-# LANGUAGE TemplateHaskell #-}
module Data.Token
  ( Token'(..)
  , Token
  , Id
  , expiredAt
  , issuedAt
  , userId
  , uuid
  ) where

import Import

data Token' f = Token
  { _userId    :: C f Text
  , _issuedAt  :: C f UTCTime
  , _expiredAt :: C f (Maybe UTCTime)
  , _uuid      :: C f Text
  } deriving Generic

makeLenses ''Token'

type Token = Token' Identity
type Id    = PrimaryKey Token' Identity

deriving instance Show Token
deriving instance Eq   Token

instance Ord Token where
  compare = compare `on` _issuedAt

instance Beamable Token'

instance Table Token' where
  data PrimaryKey Token' f = Id (C f Text)
    deriving (Generic, Beamable)
  primaryKey = Id . _uuid
