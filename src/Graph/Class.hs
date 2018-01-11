{-# LANGUAGE
    DataKinds
  , DeriveGeneric
  , DuplicateRecordFields
  , OverloadedStrings
  , PatternSynonyms
  , TypeOperators
  , ViewPatterns
#-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.Class where

import           Data.Aeson                  as Aeson
import qualified Data.HashMap.Lazy           as HM
import           Data.Int                    (Int32)
import qualified Data.Map                    as M
import           Data.Scientific             (floatingOrInteger)
import           Data.Text                   (Text)
import           GraphQL.API
import           GraphQL.Resolver
import           GraphQL.Internal.Syntax.AST (Variable(..))
import           GraphQL.Value
import           GraphQL.Value.FromValue     (FromValue(..))
import           GraphQL.Value.ToValue       (ToValue(..))

import Core
import Graph.Schema

instance GraphQLEnum BranchAccess

instance FromJSON QueryData where
  parseJSON = Aeson.withObject "QueryData" $ \o -> QueryData
    <$> o .:? "operationName" .!= Operation Nothing
    <*> o .: "query"
    <*> o .:? "variables" .!= Variables mempty

instance FromJSON Name where
  parseJSON (Aeson.String str) = case makeName str of 
    Right name -> return name
    Left   err -> fail $ show err
  parseJSON _ = fail "expected name to be a string"

instance FromJSON Operation where
  parseJSON (Aeson.String "") = return $ Operation Nothing
  parseJSON Aeson.Null = return $ Operation Nothing
  parseJSON v = (Operation . Just) <$> parseJSON v

instance FromJSON Variables where
  parseJSON = withObject "VariableValues" $ \vs -> do
    converted <- mapM convert $ HM.toList vs
    return . Variables . M.fromList $ converted
    where 
      convert (k, v) = do
        name <- case makeName k of
          Right n  -> return $ Variable n
          Left err -> fail $ show err
        return (name, toValue v)

instance ToValue Aeson.Value where
  toValue (Aeson.Number number) = case floatingOrInteger number of
    Left float -> toValue (float :: Double)
    Right int  -> toValue (int   :: Int32)
  toValue (Aeson.String t)  = toValue t
  toValue (Aeson.Bool   b)  = toValue b
  toValue (Aeson.Object o)  = case objectFromList . map convert $ HM.toList o of
    Nothing -> error "Could not convert object"
    Just o' -> toValue o'
    where
      convert (key, val) =
        let Right name = makeName key
        in (name, toValue val)
  toValue (Aeson.Array  _a) = error "Input is an array"
  toValue Aeson.Null        = toValue (Nothing :: Maybe Text)

instance ToValue BranchAccess where
  toValue = ValueEnum . enumToValue

instance FromValue CreateSpaceInput
instance HasAnnotatedInputType CreateSpaceInput
instance Defaultable CreateSpaceInput where
  defaultFor _ = error "No default for CreateSpaceInput"

instance FromValue CreatePropertyInput
instance HasAnnotatedInputType CreatePropertyInput
instance Defaultable CreatePropertyInput where
  defaultFor _ = error "No default for CreatePropertyInput"

instance FromValue AssertTraitInput
instance HasAnnotatedInputType AssertTraitInput
instance Defaultable AssertTraitInput where
  defaultFor _ = error "No default for AssertTraitInput"

instance FromValue AssertTheoremInput
instance HasAnnotatedInputType AssertTheoremInput
instance Defaultable AssertTheoremInput where
  defaultFor _ = error "No default for AssertTheoremInput"

instance FromValue ResetBranchInput
instance HasAnnotatedInputType ResetBranchInput
instance Defaultable ResetBranchInput where
  defaultFor _ = error "No default for ResetBranchInput"

instance FromValue UpdateSpaceInput
instance HasAnnotatedInputType UpdateSpaceInput
instance Defaultable UpdateSpaceInput where
  defaultFor _ = error "No default for UpdateSpaceInput"

instance FromValue UpdatePropertyInput
instance HasAnnotatedInputType UpdatePropertyInput
instance Defaultable UpdatePropertyInput where
  defaultFor _ = error "No default for UpdatePropertyInput"

instance FromValue UpdateTheoremInput
instance HasAnnotatedInputType UpdateTheoremInput
instance Defaultable UpdateTheoremInput where
  defaultFor _ = error "No default for UpdateTheoremInput"

instance FromValue PatchInput
instance HasAnnotatedInputType PatchInput
instance Defaultable PatchInput where
  defaultFor _ = error "No default for CreateSpaceInput"
