{-# LANGUAGE TypeApplications #-}
module Graph.Introspection (
  renderSchema
) where

import Import

import qualified Data.List.NonEmpty as NE
import qualified Data.Map  as M
import qualified Data.Text as T
import           GraphQL.API
import           GraphQL.Internal.Name
import           GraphQL.Internal.Schema
-- import qualified GraphQL.Internal.Syntax.AST as AST
-- import           GraphQL.Internal.Syntax.Encoder

import Graph.Schema (Root)
import Graph.Class  ()

-- schemaTypes :: Map Name TypeDefinition
-- schemaTypes =
--   let Right definition = getDefinition @Root
--   in getDefinedTypes definition
-- 
-- renderSchema :: Text
-- renderSchema = schemaDocument $ AST.SchemaDocument $ map typeDefToAST $ M.elems schemaTypes

renderSchema :: Either SchemaError Text
renderSchema = case getDefinition @Root of
  Left err -> Left err
  Right definition ->
    Right $ T.intercalate "\n\n" $ map renderTypeDefinition $ M.toList $ getDefinedTypes definition

renderTypeDefinition :: (Name, TypeDefinition) -> Text
renderTypeDefinition (name, typeDef) = case typeDef of
  (TypeDefinitionObject (ObjectTypeDefinition _ _ fieldDefs)) -> T.intercalate "\n" $
    [ "type " <> (unName name) <> " {"
    , T.intercalate "\n" (NE.toList $ map renderFieldDefinition fieldDefs)
    , "}" 
    ]
  _ -> error "renderTypeDefinition"

renderFieldDefinition :: FieldDefinition -> Text
renderFieldDefinition (FieldDefinition name _ atype) = 
  "  " <> unName name <> ": " <> renderAnnotatedType atype

renderAnnotatedType :: AnnotatedType GType -> Text
renderAnnotatedType (TypeNamed t) = renderType t
renderAnnotatedType (TypeList (ListType t)) = "[" <> (renderAnnotatedType t) <> "]"
renderAnnotatedType (TypeNonNull (NonNullTypeNamed t)) = (renderType t) <> "!"
renderAnnotatedType (TypeNonNull (NonNullTypeList (ListType t))) = "[" <> (renderAnnotatedType t) <> "]!"

renderType :: GType -> Text
renderType (DefinedType typeDef) = unName $ getName typeDef
renderType (BuiltinType typeDef) = unName $ getName typeDef
