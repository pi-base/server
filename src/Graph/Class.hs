{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Graph.Class where

import Protolude

import           Control.Monad               (fail)
import           Data.Aeson                  ((.:), (.:?), (.!=))
import qualified Data.Aeson                  as Aeson
import qualified Data.ByteString.Lazy        as BSL
import qualified Data.HashMap.Lazy           as HM
import           Data.Int                    (Int32)
import qualified Data.List.NonEmpty          as NonEmpty
import qualified Data.Map                    as M
import           Data.Scientific             (floatingOrInteger)
import           Data.String                 (IsString(..))
import           Data.Text                   (Text, pack)
import qualified Data.Vector                 as V
import           GraphQL.API
import qualified GraphQL.Internal.OrderedMap as OM
import           GraphQL.Internal.Syntax.AST (Variable(..))
import           GraphQL.Internal.Schema
import           GraphQL.Value

import Core
import Graph.Schema

instance IsString GraphQL.Value.String where
  fromString = GraphQL.Value.String . pack

instance GraphQLEnum BranchAccess
instance GraphQLEnum CitationType

instance forall t. (HasAnnotatedInputType t) => HasAnnotatedInputType [t] where
  getAnnotatedInputType = TypeList . ListType <$> getAnnotatedInputType @t

instance Aeson.FromJSON QueryData where
  parseJSON = Aeson.withObject "QueryData" $ \o -> QueryData
    <$> o .:? "operationName" .!= Operation Nothing
    <*> o .: "query"
    <*> o .:? "variables" .!= Variables mempty

instance Aeson.FromJSON Name where
  parseJSON (Aeson.String str) = case makeName str of 
    Right name -> return name
    Left   err -> fail $ show err
  parseJSON _ = fail "expected name to be a string"

instance Aeson.FromJSON Operation where
  parseJSON (Aeson.String "") = return $ Operation Nothing
  parseJSON Aeson.Null = return $ Operation Nothing
  parseJSON v = (Operation . Just) <$> Aeson.parseJSON v

instance FromJSON Variables where
  parseJSON = Aeson.withObject "VariableValues" $ \vs -> do
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
    Nothing -> panic "Could not convert object"
    Just o' -> toValue o'
    where
      convert (key, val) =
        let Right name = makeName key
        in (name, toValue val)
  toValue (Aeson.Array a) = ValueList' $ List' $ map toValue $ V.toList a
  toValue Aeson.Null      = toValue (Nothing :: Maybe Text)

instance ToValue BranchAccess where
  toValue = ValueEnum . enumToValue

instance FromValue CreateSpaceInput where
  fromValue = withObject "CreateSpaceInput" $ \o -> CreateSpaceInput
    <$> field "name"        o
    <*> field "description" o
    <*> field "references"  o
instance HasAnnotatedInputType CreateSpaceInput where
  getAnnotatedInputType = inputType "CreateSpaceInput"
    [ ("name",        BuiltinInputType GString)
    , ("description", BuiltinInputType GString)
    , ("references",  BuiltinInputType GString)
    ]
instance Defaultable CreateSpaceInput where
  defaultFor _ = panic "No default for CreateSpaceInput"

instance FromValue CreatePropertyInput where
  fromValue = withObject "CreatePropertyInput" $ \o -> CreatePropertyInput
    <$> field "name"        o
    <*> field "description" o
    <*> field "references"  o
instance HasAnnotatedInputType CreatePropertyInput where
  getAnnotatedInputType = inputType "CreatePropertyInput"
    [ ("name",        BuiltinInputType GString)
    , ("description", BuiltinInputType GString)
    , ("references",  BuiltinInputType GString)
    ]
instance Defaultable CreatePropertyInput where
  defaultFor _ = panic "No default for CreatePropertyInput"

instance FromValue CitationType where
  fromValue (ValueString s) = case s of
    "doi"       -> return DOICitation
    "mr"        -> return MRCitation
    "wikipedia" -> return WikiCitation
    _           -> wrongType "String" s
  fromValue v = wrongType "String" v

instance FromValue Core.Citation where
  fromValue (ValueObject o) = Citation
    <$> field "name" o
    <*> field "type" o
    <*> field "ref" o
  fromValue v = wrongType "Object" v
instance HasAnnotatedInputType Core.Citation where
  getAnnotatedInputType = inputType "Citation"
    [ ("name", BuiltinInputType GString)
    , ("type", BuiltinInputType GString)
    , ("ref",  BuiltinInputType GString)
    ]

instance FromValue AssertTraitInput where
  fromValue = withObject "AssertTraitInput" $ \o -> AssertTraitInput
    <$> field "spaceId"     o
    <*> field "propertyId"  o
    <*> field "value"       o
    <*> field "description" o
    <*> field "references"  o
instance HasAnnotatedInputType AssertTraitInput where
  getAnnotatedInputType = inputType "AssertTraitInput"
    [ ("spaceId",     BuiltinInputType GID)
    , ("propertyId",  BuiltinInputType GID)
    , ("value",       BuiltinInputType GBool)
    , ("description", BuiltinInputType GString)
    , ("references",  BuiltinInputType GString)
    ]
instance Defaultable AssertTraitInput where
  defaultFor _ = panic "No default for AssertTraitInput"

instance FromValue AssertTheoremInput where
  fromValue = withObject "AssertTheoremInput" $ \o -> AssertTheoremInput
    <$> field "antecedent"  o
    <*> field "consequent"  o
    <*> field "description" o
    <*> field "references"  o
instance HasAnnotatedInputType AssertTheoremInput where
  getAnnotatedInputType = inputType "AssertTraitInput"
    [ ("uid",         BuiltinInputType GID)
    , ("antecedent",  BuiltinInputType GString)
    , ("consequent",  BuiltinInputType GString)
    , ("description", BuiltinInputType GString)
    , ("references",  BuiltinInputType GString)
    ]
instance Defaultable AssertTheoremInput where
  defaultFor _ = panic "No default for AssertTheoremInput"

instance FromValue ResetBranchInput
instance HasAnnotatedInputType ResetBranchInput
instance Defaultable ResetBranchInput where
  defaultFor _ = panic "No default for ResetBranchInput"

instance FromValue BranchInput
instance HasAnnotatedInputType BranchInput
instance Defaultable BranchInput where
  defaultFor _ = panic "No default for ResetBranchInput"

instance FromValue UpdateSpaceInput
instance HasAnnotatedInputType UpdateSpaceInput
instance Defaultable UpdateSpaceInput where
  defaultFor _ = panic "No default for UpdateSpaceInput"

instance FromValue UpdatePropertyInput
instance HasAnnotatedInputType UpdatePropertyInput
instance Defaultable UpdatePropertyInput where
  defaultFor _ = panic "No default for UpdatePropertyInput"

instance FromValue UpdateTheoremInput
instance HasAnnotatedInputType UpdateTheoremInput
instance Defaultable UpdateTheoremInput where
  defaultFor _ = panic "No default for UpdateTheoremInput"

instance FromValue UpdateTraitInput where
  fromValue = withObject "UpdateTraitInput" $ \o -> UpdateTraitInput
    <$> field "spaceId"     o
    <*> field "propertyId"  o
    <*> field "description" o
    <*> field "references"  o
instance HasAnnotatedInputType UpdateTraitInput where
  getAnnotatedInputType = inputType "UpdateTraitInput"
    [ ("spaceId",     BuiltinInputType GID)
    , ("propertyId",  BuiltinInputType GID)
    , ("description", BuiltinInputType GString)
    , ("references",  BuiltinInputType GString)
    ]

instance Defaultable UpdateTraitInput where
  defaultFor _ = panic "No default for UpdateTraitInput"

instance FromValue PatchInput
instance HasAnnotatedInputType PatchInput
instance Defaultable PatchInput where
  defaultFor _ = panic "No default for PatchInput"

instance HasAnnotatedInputType (Id a) where
  getAnnotatedInputType = Right $ TypeNamed $ BuiltinInputType GString
instance FromValue (Id a) where
  fromValue (ValueString (GraphQL.Value.String s)) = Right $ Id s
  fromValue v = wrongType "String" v

instance HasAnnotatedInputType (Formula PropertyId) where
  getAnnotatedInputType = Right $ TypeNamed $ BuiltinInputType GString
instance FromValue (Formula PropertyId) where
  fromValue (ValueString (GraphQL.Value.String s)) = case Aeson.eitherDecode $ BSL.fromStrict $ encodeUtf8 s of
    Left err -> Left $ "Could not parse formula: " <> show err
    Right f  -> Right $ Id <$> f
  fromValue v = wrongType "String" v

wrongType :: Show a => Text -> a -> Either Text b
wrongType expected value = Left $ "Wrong type, should be: `" <> expected <> "` but is: `" <> show value <> "`"

field :: FromValue a => Name -> Object' ConstScalar -> Either Text a
field name (Object' fieldMap) = case OM.lookup name fieldMap of
  Nothing -> Left $ "Key not found: " <> show name
  Just v  -> fromValue v

inputType :: Name -> [(Name, InputType)] -> Either a (AnnotatedType InputType)
inputType name pairs = Right . TypeNamed . DefinedInputType . InputTypeDefinitionObject . InputObjectTypeDefinition name . NonEmpty.fromList
  $ map (\(fieldName, fieldType) -> InputObjectFieldDefinition fieldName (TypeNamed fieldType) Nothing) pairs

withObject :: Show t => Text -> (Object' t -> Either Text b) -> Value' t -> Either Text b
withObject _ parser (ValueObject o) = parser o
withObject name _ v = wrongType (name <> " should be an Object") v
