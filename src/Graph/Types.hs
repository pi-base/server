{-# LANGUAGE TemplateHaskell #-}
module Graph.Types
  ( module Graph.Types
  ) where

import Protolude

import Control.Lens                (makeLenses)
import Data.Aeson                  (ToJSON)
import Database.Persist            (Entity)
import GraphQL                     (Schema, VariableValues)
import GraphQL.Internal.Validation (QueryDocument, VariableValue)
import GraphQL.Value               as Graph.Types (Name(..))

import           Model
import qualified Services.Github.Types as Github

type Query = QueryDocument VariableValue

newtype Operation = Operation { unOp :: Maybe Name }
  deriving (Eq, Ord, Show, Generic)

newtype Variables = Variables { unVar :: VariableValues }
  deriving (Eq, Show, Generic)

data QueryData = QueryData
  { operation :: Operation
  , query     :: Text
  , variables :: Variables
  } deriving (Show, Eq, Generic)

data QueryCache = QueryCache
  { root           :: FilePath
  , querySchema    :: Schema
  , mutationSchema :: Schema
  , queries        :: Map Text Query
  } deriving (Show, Eq, Generic)

data Settings = Settings
  { _queryStore :: Maybe FilePath
  } deriving (Show, Eq, Generic)

makeLenses ''Settings

instance ToJSON Settings

data Context = Context
  { _currentUser    :: Maybe (Entity User)
  , _githubSettings :: Github.Settings
  } deriving (Show, Generic)

makeLenses ''Context
