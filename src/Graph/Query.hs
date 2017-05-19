{-# LANGUAGE TypeApplications #-}
module Graph.Query
  ( GQuery
  , query
  ) where

import Prelude

import Data.Aeson                  as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map          as Map
import Data.Int                    (Int32)
import Data.Scientific             (floatingOrInteger)
import Data.Text                   (Text)
import Data.Text.Lazy              (toStrict)
import GraphQL
import GraphQL.API
import GraphQL.Resolver            (Handler)
import GraphQL.Value               (makeName)
import GraphQL.Value.ToValue       (ToValue(..), toValue)
import GraphQL.Internal.Syntax.AST (Name(..), Variable(..))

import Graph.Types (QueryRoot)

data GQuery = GQuery
  { gOperation :: Maybe Name
  , gQuery     :: Text
  , gVariables :: Aeson.Object
  }

instance FromJSON GQuery where
  parseJSON = Aeson.withObject "GQuery" $ \o -> GQuery
    <$> o .: "operationName"
    <*> o .: "query"
    <*> o .: "variables"

instance FromJSON Name where
  parseJSON (Aeson.String str) = case makeName str of
    Left err -> fail $ show err
    Right name -> return name
  parseJSON _ = fail "Name is not a string"

-- FIXME: define instance FromJSON VariableValues instead
instance ToValue Aeson.Value where
  toValue (Aeson.Object object) = error "object"
  toValue (Aeson.Array array)   = error "array"
  toValue (Aeson.String text)   = toValue text
  toValue (Aeson.Number number) = case floatingOrInteger number of
    Left float -> toValue (float :: Double)
    Right int  -> toValue (int :: Int32)
  toValue (Aeson.Bool bool)     = toValue bool
  toValue Aeson.Null            = error "null"

buildVariables :: Aeson.Object -> VariableValues
buildVariables = Map.fromList . map convert . HashMap.toList
  where
    convert (key, val) = (Variable (name key), toValue val)
    name key = let Right n = makeName key in n

query :: Monad m => Handler m QueryRoot -> GQuery -> m Response
query root GQuery{..} = interpretQuery @QueryRoot root gQuery gOperation (buildVariables gVariables)
