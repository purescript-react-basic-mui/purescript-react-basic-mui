module Codegen.Write.React where

import Prelude

import Codegen.Read (DeclarationElements(..), EntityName(..), InterfaceDeclarationRec, LiteralValue(..), PropertyName(..), TSType(..), TypeMember(..), TypeParameter(..), _IdentifierName, _PropertySignature, _name, isOptional, liftEither)
import Data.Array as Array
import Data.Lens (_Just, over)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Effect (Effect)


showPropsInterface :: InterfaceDeclarationRec -> Effect String
showPropsInterface interface = do
  members <- cleanMembers interface.typeMembers
  let partition = Array.partition isOptional members 
  let optionalMembers = partition.yes
  let requiredMembers = partition.no
  pure $ if Array.length requiredMembers > 0
    then Array.intercalate "\n\n" [requiredType requiredMembers, optionalType optionalMembers]
    else singleType members
  where
    parameters | Array.length interface.typeParameters > 0 = " " <> (Array.intercalate " " $ map showTypeParameter interface.typeParameters)
    parameters = ""

    requiredType members = "type " <> interface.name <> "_required" <> parameters <> " =\n  { " <> (showMembers members) <> "\n  }"
    optionalType members = "type " <> interface.name <> "_optional" <> parameters <> " =\n  { " <> (showMembers members) <> "\n  }"
    singleType members = "type " <> interface.name <> parameters <> " =\n  { " <> (showMembers interface.typeMembers) <> "\n  }"

    showMembers :: Array TypeMember -> String
    showMembers members = Array.intercalate "  , " $ map (\member -> (showTypeMember member) <> "\n") members

cleanMembers :: Array TypeMember -> Effect (Array TypeMember)
cleanMembers members = do 
  regex <- liftEither $ Regex.regex "^[_a-z]+\\w*$" Flags.noFlags
  pure $ map (filterProp regex) members
  where
    filterProp :: Regex -> TypeMember -> TypeMember
    filterProp regex signature = do
      let lens = _PropertySignature <<< _name <<< _Just <<< _IdentifierName
      over lens (clean regex) signature

    clean :: Regex -> String -> String
    clean regex name = case (Regex.match regex name) of 
      Nothing -> "\"" <> name <> "\""
      Just _ -> name

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
showTSType tsType = do
  passthrough tsType
  where
    passthrough BooleanType = "Boolean"
    passthrough StringType = "String"
    passthrough NumberType = "Number"
    passthrough BigIntType = "Number"
    passthrough ObjectType = "(Object Foreign)"
    passthrough VoidType = "Unit"
    passthrough AnyType = "Any"
    passthrough SymbolType = "Symbol"
    passthrough UnknownType = "Unknown"
    passthrough NeverType = "Never"
    passthrough (ParenthesizedType t) = "(" <> (showTSType t) <> ")"
    passthrough (TypeReference { name, typeArguments }) | Array.length typeArguments > 0 = showEntityName name <> " " <> (Array.intercalate " " $ map showTSType typeArguments)
    passthrough (TypeReference { name, typeArguments }) = showEntityName name
    passthrough (FunctionType { typeParameters, parameters, returnType }) | Array.length typeParameters == 0 = (Array.intercalate " -> " $ map showTypeMemberAsFunctionType parameters) <> " -> " <> (showTSType returnType)
    passthrough (FunctionType { typeParameters, parameters, returnType }) = "forall " <> (Array.intercalate " " $ map showTypeParameter typeParameters) <> ". " <> (Array.intercalate " -> " $ map showTypeMemberAsFunctionType parameters) <> " -> " <> (showTSType returnType)
    passthrough (ArrayType t) = "(Array " <> (showTSType t) <> ")"
    passthrough (UnionType types) = handleUnionTypes types
    passthrough (LiteralType (LiteralStringValue value)) = value
    passthrough (LiteralType (LiteralNumericValue value)) = value
    passthrough (LiteralType (LiteralBigIntValue value)) = value
    passthrough (LiteralType (LiteralBooleanValue value)) = show value
    passthrough (TypeLiteral members) = "{ " <> (Array.intercalate ", " $ map showTypeMember members) <> " }"
    passthrough t = show t

handleUnionTypes :: Array TSType -> String
handleUnionTypes types = "Foreign" 

showEntityName :: EntityName -> String
showEntityName (Identifier name) = name
showEntityName (QualifiedName { left, right }) = (showEntityName left) <> "." <> right

showPropertyName :: PropertyName -> String
showPropertyName (IdentifierName name) = name
showPropertyName (StringLiteral name) = name
showPropertyName (NumericLiteral name) = show (show name)
