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
import GraphQL
import GraphQL.Resolver            (Handler)
import GraphQL.Value               (makeName)
import GraphQL.Value.ToValue       (ToValue(..), toValue)
import GraphQL.Internal.Syntax.AST (Name(..), Variable(..))

import Graph.Types (QueryRoot)

data Operation = Named Name | Anonymous

data GQuery = GQuery
  { gOperation :: Operation
  , gQuery     :: Text
  , gVariables :: Maybe Aeson.Object
  }

instance FromJSON GQuery where
  parseJSON = Aeson.withObject "GQuery" $ \o -> GQuery
    <$> o .: "operationName"
    <*> o .: "query"
    <*> o .:? "variables"

instance FromJSON Operation where
  parseJSON (Aeson.String "") = return Anonymous
  parseJSON (Aeson.String str) = case makeName str of
    Left err -> fail $ show err
    Right name -> return $ Named name
  parseJSON Aeson.Null = return Anonymous
  parseJSON _ = fail "Name is not a string"

-- FIXME: define instance FromJSON VariableValues instead
instance ToValue Aeson.Value where
  toValue (Aeson.Number number) = case floatingOrInteger number of
    Left float -> toValue (float :: Double)
    Right int  -> toValue (int   :: Int32)
  toValue (Aeson.String text)   = toValue text
  toValue (Aeson.Bool bool)     = toValue bool
  toValue (Aeson.Object _obj)   = error "object"
  toValue (Aeson.Array _arr)    = error "array"
  toValue Aeson.Null            = error "null"

buildVariables :: Maybe Aeson.Object -> VariableValues
buildVariables (Just hm) = Map.fromList . map convert $ HashMap.toList hm
  where
    convert (key, val) = (Variable (name key), toValue val)
    name key = let Right n = makeName key in n
buildVariables Nothing = mempty

query :: Monad m => Handler m QueryRoot -> GQuery -> m Response
query root GQuery{..} = interpretQuery @QueryRoot root gQuery operation variables
  where
    operation = case gOperation of
      Named name -> Just name
      Anonymous  -> Nothing

    variables = buildVariables gVariables

