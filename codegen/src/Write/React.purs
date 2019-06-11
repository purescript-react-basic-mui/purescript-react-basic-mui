module Codegen.Write.React where

import Prelude

import Codegen.Read (DeclarationElements(..), EntityName(..), InterfaceDeclarationRec, LiteralValue(..), PropertyName(..), TSType(..), TypeMember(..), TypeParameter(..), isOptional)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String as String


showPropsInterface :: InterfaceDeclarationRec -> String
showPropsInterface interface = do
  let partition = Array.partition isOptional interface.typeMembers
  let optionalMembers = partition.yes
  let requiredMembers = partition.no
  if Array.length requiredMembers > 0
    then Array.intercalate "\n\n" [requiredType requiredMembers, optionalType optionalMembers]
    else singleType 
  where
    parameters | Array.length interface.typeParameters > 0 = " " <> (Array.intercalate " " $ map showTypeParameter interface.typeParameters)
    parameters = ""

    requiredType members = "type " <> interface.name <> "_required" <> parameters <> " =\n  { " <> (showMembers members) <> "\n  }"
    optionalType members = "type " <> interface.name <> "_optional" <> parameters <> " =\n  { " <> (showMembers members) <> "\n  }"
    singleType = "type " <> interface.name <> parameters <> " =\n  { " <> (showMembers interface.typeMembers) <> "\n  }"

    showMembers :: Array TypeMember -> String
    showMembers members = Array.intercalate "  , " $ map (\member -> (showTypeMember member) <> "\n") members

showDeclarationElements :: DeclarationElements -> String
showDeclarationElements (InterfaceDeclaration { name, typeParameters, typeMembers }) = 
  "type " <> name <> parameters <> " =\n  { " <> members <> "\n  }"
  where
    parameters | Array.length typeParameters > 0 = " " <> (Array.intercalate " " $ map showTypeParameter typeParameters)
    parameters = ""
    members = Array.intercalate "  , " $ map (\member -> (showTypeMember member) <> "\n") typeMembers
showDeclarationElements element = show element

showTypeMember :: TypeMember -> String
showTypeMember (PropertySignature signature) = 
  (getName signature.name) <> " :: " <> fieldType
  where
    getName (Just propertyName) = showPropertyName propertyName
    getName Nothing = "NoName"
    fieldType = showTSType signature.type
showTypeMember typeMember = show typeMember

showTypeMemberAsFunctionType :: TypeMember -> String
showTypeMemberAsFunctionType (PropertySignature signature) | signature.isOptional = "(Maybe -- this doesn't seem right " <> (showTSType signature.type) <> ")"
showTypeMemberAsFunctionType (PropertySignature signature) = showTSType signature.type
showTypeMemberAsFunctionType typeMember = show typeMember


showTypeParameter :: TypeParameter -> String
showTypeParameter (TypeParameter param) = String.toLower param

showTSType :: TSType -> String
showTSType BooleanType = "Boolean"
showTSType StringType = "String"
showTSType NumberType = "Number"
showTSType BigIntType = "Number"
showTSType ObjectType = "(Object Foreign)"
showTSType VoidType = "Unit"
showTSType AnyType = "Any"
showTSType SymbolType = "Symbol"
showTSType UnknownType = "Unknown"
showTSType NeverType = "Never"
showTSType (ParenthesizedType t) = "(" <> (showTSType t) <> ")"
showTSType (TypeReference { name, typeArguments }) | Array.length typeArguments > 0 = showEntityName name <> " " <> (Array.intercalate " " $ map showTSType typeArguments)
showTSType (TypeReference { name, typeArguments }) = showEntityName name
showTSType (FunctionType { typeParameters, parameters, returnType }) | Array.length typeParameters == 0 = (Array.intercalate " -> " $ map showTypeMemberAsFunctionType parameters) <> " -> " <> (showTSType returnType)
showTSType (FunctionType { typeParameters, parameters, returnType }) = "forall " <> (Array.intercalate " " $ map showTypeParameter typeParameters) <> ". " <> (Array.intercalate " -> " $ map showTypeMemberAsFunctionType parameters) <> " -> " <> (showTSType returnType)
showTSType (ArrayType t) = "(Array " <> (showTSType t) <> ")"
showTSType (UnionType types) = handleUnionTypes types
showTSType (LiteralType (LiteralStringValue value)) = value
showTSType (LiteralType (LiteralNumericValue value)) = value
showTSType (LiteralType (LiteralBigIntValue value)) = value
showTSType (LiteralType (LiteralBooleanValue value)) = show value
showTSType t = show t

handleUnionTypes :: Array TSType -> String
handleUnionTypes types = do
  "(Variant (" <> typeStrs <> "))"
  where 
    typeStrs = 
      Array.intercalate ", " $ types <#> (\t -> do
      case t of 
        LiteralType _ -> do
          let str = showTSType t 
          str <> " :: Unit"
        _                 -> do
          let str = showTSType t
          str <> " :: " <> str
      ) 


showEntityName :: EntityName -> String
showEntityName (Identifier name) = name
showEntityName (QualifiedName { left, right }) = (showEntityName left) <> "." <> right

showPropertyName :: PropertyName -> String
showPropertyName (IdentifierName name) = name
showPropertyName (StringLiteral name) = name
showPropertyName (NumericLiteral name) = show (show name)
