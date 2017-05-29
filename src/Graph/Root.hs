{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Graph.Root
  ( query
  ) where

import Graph.Import
import qualified Import (Handler)

import Data.Aeson                  as Aeson
import qualified Data.HashMap.Lazy as HashMap
import qualified Data.Map          as Map
import Data.Int                    (Int32)
import Data.Scientific             (floatingOrInteger)
import Data.Text                   (Text)
import GraphQL                     (VariableValues)
import GraphQL.Value               (makeName)
import GraphQL.Value.ToValue       (ToValue(..), toValue)
import GraphQL.Internal.Syntax.AST (Name(..), Variable(..))

import Core (Version, Maybe)

import Graph.Mutation as G
import Graph.Query    as G
import Graph.Types    as G

data Operation = Named Name | Anonymous

type QueryRoot = Graph.Import.Object "QueryRoot" '[]
  '[ Field "__typename" Text
   , Argument "version" (Maybe Version) :> Field "viewer" Viewer
   , Field "me" G.User
   -- Mutations
   -- , Argument "uid" Text :> Argument "description" Text :> Field "updateSpace" Space
   , Argument "input" SpaceInput :> Field "updateSpace" Space
   , Argument "uid" Text :> Argument "description" Text :> Field "updateProperty" Property
   ]

data QueryData = QueryData
  { qOperation :: Operation
  , qQuery     :: Text
  , qVariables :: Maybe Aeson.Object
  }

queryRoot :: G QueryRoot
queryRoot = pure $ pure "Query"
  :<> G.viewerR
  :<> G.userR
  :<> G.updateSpace
  :<> G.updateProperty

query :: QueryData -> Import.Handler Aeson.Value
query q = do
  response <- runQuery queryRoot q
  return . Aeson.toJSON $ toValue response

runQuery :: Monad m => Handler m QueryRoot -> QueryData -> m Response
runQuery root QueryData{..} = interpretQuery @QueryRoot root qQuery operation (buildVariables qVariables)
  where
    operation = case qOperation of
      Named name -> Just name
      Anonymous  -> Nothing

instance FromJSON QueryData where
  parseJSON = Aeson.withObject "QueryData" $ \o -> QueryData
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
  toValue (Aeson.String t)  = toValue t
  toValue (Aeson.Bool   b)  = toValue b
  toValue (Aeson.Object _o) = error "object"
  toValue (Aeson.Array  _a) = error "array"
  toValue Aeson.Null        = error "null"

buildVariables :: Maybe Aeson.Object -> VariableValues
buildVariables (Just hm) = Map.fromList . map convert $ HashMap.toList hm
  where
    convert (key, val) = (Variable (name key), toValue val)
    name key = let Right n = makeName key in n
buildVariables Nothing = mempty

