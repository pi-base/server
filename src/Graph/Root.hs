{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , TemplateHaskell
  , TypeApplications
  , TypeOperators
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.Root
  ( Graph.Root.query
  , exec
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
import GraphQL.Value               (makeName, objectFromList)
import GraphQL.Value.ToValue       (ToValue(..), toValue)
import GraphQL.Internal.Syntax.AST (Variable(..))
import GraphQL.Internal.Validation (QueryDocument, VariableValue)

import Core (Maybe)

import Graph.Mutations
import Graph.Queries   as G
import Graph.Types     as G


patchMutations :: MonadGraph m => PatchInput -> Handler m PatchMutation
patchMutations PatchInput{..} = do
  $(logInfo) $ "Patching branch " <> branch <> " at sha " <> sha
  -- TODO:
  --   401 if user does not have write access to branch
  --   409 if sha does not match head of branch
  return $ pure "PatchMutation"
    :<> createSpace
    :<> createProperty
    :<> updateSpace
    :<> updateProperty
    :<> updateTheorem
    :<> assertTrait
    :<> assertTheorem

rootHandler :: MonadGraph m => Handler m Root
rootHandler = pure $ pure "Query"
  :<> G.viewer
  :<> G.user
  -- Mutations
  :<> patchMutations
  :<> resetBranch
  :<> testReset

query :: QueryData -> Import.Handler Aeson.Value
query q = (Aeson.toJSON . toValue) <$> runQuery rootHandler q

runQuery :: Monad m => Handler m Root -> QueryData -> m Response
runQuery root QueryData{..} = interpretQuery @Root root query (op operation) (buildVariables variables)
  where
    op (Named name) = Just name
    op _ = Nothing

exec :: MonadGraph m => QueryDocument VariableValue -> Maybe Aeson.Object -> m Aeson.Value
exec q vars = (Aeson.toJSON . toValue) <$> executeQuery @Root rootHandler q Nothing (buildVariables vars)

instance FromJSON QueryData where
  parseJSON = Aeson.withObject "QueryData" $ \o -> QueryData
    <$> o .:? "operationName" .!= Anonymous
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
  toValue (Aeson.Object o)  = case objectFromList . map convert $ HashMap.toList o of
    Nothing -> error "Could not convert object"
    Just o' -> toValue o'
    where
      convert (key, val) =
        let Right name = makeName key
        in (name, toValue val)
  toValue (Aeson.Array  _a) = error "Input is an array"
  toValue Aeson.Null        = toValue (Nothing :: Maybe Text)

buildVariables :: Maybe Aeson.Object -> VariableValues
buildVariables (Just hm) = Map.fromList . map convert $ HashMap.toList hm
  where
    convert (key, val) =
      let Right name = makeName key
      in (Variable name, toValue val)
buildVariables Nothing = mempty
