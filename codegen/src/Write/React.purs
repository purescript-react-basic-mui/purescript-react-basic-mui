module Codegen.Write.React where

import Prelude

import Codegen.Read (DeclarationElements(..), DeclarationSourceFile(..), EntityName(..), InterfaceDeclarationRec, LiteralValue(..), PropertyName(..), TSType(..), TypeMember(..), TypeParameter(..), VariableDeclaration(..), _IdentifierName, _PropertySignature, _name, isOptional, liftEither)
import Data.Array as Array
import Data.FoldableWithIndex as Fold
import Data.Lens (_Just, over)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)

type Replacements = Array (Tuple Regex String)

showDeclarationSourceFile :: DeclarationSourceFile -> Effect String
showDeclarationSourceFile (DeclarationSourceFile rec) = do
  replacements  <- buildReplacements
  elements      <- traverse (showDeclarationElements replacements) rec.elements
  pure $ Array.intercalate "\n\n" elements

showPropsInterface :: Replacements -> InterfaceDeclarationRec -> Effect String
showPropsInterface replacements interface = do
  members <- cleanMembers interface.typeMembers
  let partition = Array.partition isOptional members 
  let optionalMembers = partition.yes
  let requiredMembers = partition.no
  pure $ if Array.length requiredMembers > 0
    then Array.intercalate "\n\n" [(requiredType requiredMembers), (optionalType optionalMembers)]
    else singleType members
  where
    parameters | Array.length interface.typeParameters > 0 = " " <> (Array.intercalate " " $ map showTypeParameter interface.typeParameters)
    parameters = ""

    requiredType members = "type " <> interface.name <> "_required" <> parameters <> " =\n  { " <> (showMembers members) <> "\n  }"
    optionalType members = "type " <> interface.name <> "_optional" <> parameters <> " =\n  { " <> (showMembers members) <> "\n  }"
    singleType members = "type " <> interface.name <> parameters <> " =\n  { " <> (showMembers interface.typeMembers) <> "\n  }"

    showMembers :: Array TypeMember -> String
    showMembers members = Array.intercalate "  , " $ map (\member -> (showTypeMember replacements member) <> "\n") members

buildReplacements :: Effect Replacements
buildReplacements = do
   traverse createRegexps 
    [ Tuple (Regex.regex "^React\\.ReactNode$" Flags.noFlags) "JSX"
    , Tuple (Regex.regex "^ReactNode$" Flags.noFlags) "JSX"
    , Tuple (Regex.regex "^CSSProperties$" Flags.noFlags) "CSS"
    , Tuple (Regex.regex "^React\\.ElementType.*" Flags.noFlags) "JSX"
    , Tuple (Regex.regex ".*EventHandler.*" Flags.noFlags) "EventHandler"
    , Tuple (Regex.regex ".*RefType.*" Flags.noFlags) "Ref"
    , Tuple (Regex.regex "^Partial ClassNameMap ClassKey" Flags.noFlags) "String"
    ]
  where
    createRegexps (Tuple either str) = liftEither either <#> \regex -> Tuple regex str

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

showDeclarationElements :: Replacements -> DeclarationElements -> Effect String
showDeclarationElements replacements (InterfaceDeclaration { name, typeParameters, typeMembers }) = 
  pure $ "type " <> name <> parameters <> " =\n  { " <> members <> "\n  }"
  where
    parameters | Array.length typeParameters > 0 = " " <> (Array.intercalate " " $ map showTypeParameter typeParameters)
    parameters = ""
    members = Array.intercalate "  , " $ map (\member -> (showTypeMember replacements member) <> "\n") typeMembers

showDeclarationElements replacements (elem @ (FunctionElement { name: (Just name), fullyQualifiedName, typeParameters, parameters, returnType })) | Array.length parameters > 0 = do
  pure $ "foreign import " <> name <> " :: forall " <> (Array.intercalate " " $ map showTypeParameter typeParameters ) <> " . " <> (Array.intercalate " -> " $ map (showTypeMember replacements) parameters) <> " -> " <> (showTSType replacements returnType)

showDeclarationElements replacements (elem @ (FunctionElement { name: (Just name), fullyQualifiedName, typeParameters, parameters, returnType })) = do
  pure $ "foreign import " <> name <> " :: " <> (Array.intercalate " -> " $ map (showTypeMember replacements) parameters) <> " -> " <> (showTSType replacements returnType)

showDeclarationElements replacements (FunctionElement { name: Nothing }) = pure "" 

showDeclarationElements replacements (TypeAliasDeclaration rec) | Array.length rec.typeParameters > 0 = pure $ 
  "type " <> rec.name <> " " <> (Array.intercalate " " $ map showTypeParameter rec.typeParameters) <> " = " <> (showTSType replacements rec.type)

showDeclarationElements replacements (TypeAliasDeclaration rec) = pure $ "type " <> rec.name <> " = " <> (showTSType replacements rec.type)

showDeclarationElements replacements (VariableStatement variables) = do 
  vars <- traverse handleVar variables
  pure $ Array.intercalate "\n\n" vars
  where
    handleVar (VariableDeclaration dec) = do
      lower <- lowerCaseFirst dec.name
      pure (lower <> " :: " <> (showTSType replacements dec.type))

showDeclarationElements _ element = pure $ show element

lowerCaseFirst :: String -> Effect String
lowerCaseFirst str = do
  regex <- liftEither $ Regex.regex "^(.)" Flags.noFlags
  let replaced = Regex.replace' regex (\s _ -> String.toLower s) str
  pure replaced

showTypeMember :: Replacements -> TypeMember -> String
showTypeMember replacements (PropertySignature signature) | isJust signature.name = 
  (getName signature.name) <> " :: " <> fieldType
  where
    getName (Just propertyName) = showPropertyName propertyName
    getName Nothing = "NoName"
    fieldType = showTSType replacements signature.type
showTypeMember replacements (PropertySignature signature) = showTSType replacements signature.type
showTypeMember _ typeMember = show typeMember


showTypeMemberAsFunctionType :: Replacements -> TypeMember -> String
showTypeMemberAsFunctionType replacements (PropertySignature signature) | signature.isOptional = "(Undefinable " <> (showTSType replacements signature.type) <> ")"
showTypeMemberAsFunctionType replacements (PropertySignature signature) = showTSType replacements signature.type
showTypeMemberAsFunctionType replacements typeMember = show typeMember

showTypeParameter :: TypeParameter -> String
showTypeParameter (TypeParameter param) = String.toLower param


showTSType :: Replacements ->  TSType -> String
showTSType replacements tsType = 
  replaceTypeString replacements $ passthrough tsType
  where
    passthrough BooleanType = "Boolean"
    passthrough StringType = "String"
    passthrough NumberType = "Number"
    passthrough BigIntType = "Number"
    passthrough ObjectType = "(Object Foreign)"
    passthrough VoidType = "Unit"
    passthrough AnyType = "Foreign"
    passthrough SymbolType = "Foreign"
    passthrough UnknownType = "Foreign"
    passthrough NeverType = "Foreign"
    passthrough (ParenthesizedType t) = "(" <> (showTSType replacements t) <> ")"
    passthrough (TypeReference { name, typeArguments }) | Array.length typeArguments > 0 = showEntityName name <> " " <> (Array.intercalate " " $ map (showTSType replacements) typeArguments)
    passthrough (TypeReference { name, typeArguments }) = showEntityName name
    passthrough (FunctionType { typeParameters, parameters, returnType }) | Array.length typeParameters == 0 = (Array.intercalate " -> " $ map (showTypeMemberAsFunctionType replacements) parameters) <> " -> " <> (showTSType  replacements returnType)
    passthrough (FunctionType { typeParameters, parameters, returnType }) = "forall " <> (Array.intercalate " " $ map showTypeParameter typeParameters) <> ". " <> (Array.intercalate " -> " $ map (showTypeMemberAsFunctionType replacements ) parameters) <> " -> " <> (showTSType replacements returnType)
    passthrough (ArrayType t) = "(Array " <> (showTSType replacements t) <> ")"
    passthrough (UnionType types) = handleUnionTypes types
    passthrough (LiteralType (LiteralStringValue value)) = "String"
    passthrough (LiteralType (LiteralNumericValue value)) = "Number"
    passthrough (LiteralType (LiteralBigIntValue value)) = "Number"
    passthrough (LiteralType (LiteralBooleanValue value)) = "Boolean"
    passthrough (TypeLiteral members) = "{ " <> (Array.intercalate ", " $ map (showTypeMember replacements) members) <> " }"
    passthrough t = show t


replaceTypeString :: Replacements -> String -> String
replaceTypeString replacements tpe = do
  fromMaybe tpe $ replacements # Fold.foldlDefault isMatch Nothing
  where
    isMatch :: Maybe String -> Tuple Regex String -> Maybe String
    isMatch Nothing (Tuple regex replacement) = Regex.match regex tpe >>= (const $ Just replacement)
    isMatch (Just match) _ = Just match


handleUnionTypes :: Array TSType -> String
handleUnionTypes types = "Foreign" 

showEntityName :: EntityName -> String
showEntityName (Identifier name) = name
showEntityName (QualifiedName { left, right }) = (showEntityName left) <> "." <> right

showPropertyName :: PropertyName -> String
showPropertyName (IdentifierName name) = name
showPropertyName (StringLiteral name) = name
showPropertyName (NumericLiteral name) = show (show name)
