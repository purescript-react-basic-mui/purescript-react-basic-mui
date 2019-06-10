module Codegen.Read where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep as GR
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Fold, Lens', Prism', Traversal', prism', toListOf, traversed)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo)
import Data.String as String
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Exception (throw)
import Foreign (Foreign)
import Foreign.Generic (class Decode, decode, defaultOptions, genericDecode)


type DeclarationSourceFileRec = { fileName :: String, declarationModuleElements :: Array DeclarationModuleElements }

data DeclarationSourceFile 
  = DeclarationSourceFile DeclarationSourceFileRec

_DeclarationSourceFile :: Prism' DeclarationSourceFile DeclarationSourceFileRec
_DeclarationSourceFile = prism' DeclarationSourceFile case _ of 
  DeclarationSourceFile rec -> Just rec

_declarationModuleElements :: ∀ r. Lens' { declarationModuleElements :: Array DeclarationModuleElements | r } (Array DeclarationModuleElements)
_declarationModuleElements = prop (SProxy :: SProxy "declarationModuleElements")

data DeclarationModuleElements
  = DeclarationElement DeclarationElements
  | ImportDeclaration
  | ImportEqualsDeclaration
  | ExportDeclaration
  | ExportDefaultDeclaration
  | ExportAssignment

_DeclarationElement :: Prism' DeclarationModuleElements DeclarationElements
_DeclarationElement = prism' DeclarationElement case _ of
  DeclarationElement element -> Just element
  _ -> Nothing

type InterfaceDeclarationRec = { name :: String, typeParameters :: Array TypeParameter, typeMembers :: Array TypeMember }

data DeclarationElements
  = InterfaceDeclaration InterfaceDeclarationRec 
  | TypeAliasDeclaration { name :: String, aliasName :: Maybe String, type :: TSType }
  | AmbientDeclaration

_InterfaceDeclaration :: Prism' DeclarationElements InterfaceDeclarationRec
_InterfaceDeclaration = prism' InterfaceDeclaration case _ of 
  InterfaceDeclaration rec -> Just rec
  _ -> Nothing

toArrayOf :: forall s t a b. Fold (Endo (->) (List a)) s t a b -> s -> Array a
toArrayOf p = Array.fromFoldable <<< toListOf p

_interfaces ::  Traversal' DeclarationSourceFile InterfaceDeclarationRec
_interfaces = 
  _DeclarationSourceFile <<<
  _declarationModuleElements <<<
  traversed <<<
  _DeclarationElement <<<
  _InterfaceDeclaration

interfaces :: DeclarationSourceFile -> (Array InterfaceDeclarationRec)
interfaces declarationSourceFile = toArrayOf _interfaces declarationSourceFile

interfaceByName :: DeclarationSourceFile -> String -> Maybe InterfaceDeclarationRec
interfaceByName file interfaceName = Array.find (\{ name } -> name == interfaceName) (interfaces file)

data TypeParameter = TypeParameter String
data PropertyName 
  = IdentifierName String
  | StringLiteral String
  | NumericLiteral Number

data TypeMember
  = PropertySignature { name :: Maybe PropertyName, isOptional :: Boolean, type :: TSType }
  | CallSignature { name :: Maybe PropertyName, isOptional :: Boolean, typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
  | ConstructSignature { name :: Maybe PropertyName, isOptional :: Boolean, typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
  | MethodSignature { name :: Maybe PropertyName, isOptional :: Boolean, typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
  | IndexSignature { name :: Maybe PropertyName, isOptional :: Boolean, typeParameters :: Array TypeParameter, parameters :: Array TypeMember }

data LiteralValue = LiteralStringValue String | LiteralNumericValue String | LiteralBigIntValue String | LiteralBooleanValue Boolean

data TSType 
  = ThisType
  | BooleanType
  | StringType
  | NumberType
  | BigIntType
  | ObjectType
  | VoidType
  | NullType
  | UndefinedType
  | AnyType
  | SymbolType
  | TypeQuery
  | TypeOperator
  | UnknownType 
  | NeverType 
  | TupleType (Array TSType)
  | UnionType (Array TSType)
  | IntersectionType (Array TSType)
  | ArrayType TSType
  | TypeLiteral (Array TypeMember)
  | TypeReference { name :: EntityName }
  | ParenthesizedType TSType
  | ConstructorType { typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
  | FunctionType { typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
  | ClassType { name :: Maybe String, typeParameters :: Array TypeParameter, typeMembers :: Array TypeMember }
  | InterfaceType { name :: Maybe String, typeParameters :: Array TypeParameter, typeMembers :: Array TypeMember }
  | LiteralType LiteralValue
  | IndexAccessType { indexType :: TSType, objectType :: TSType }
  | ConditionalType { checkType :: TSType, extendsType :: TSType, trueType :: TSType, falseType :: TSType }

data EntityName 
  = Identifier String
  | QualifiedName { left :: EntityName, right :: String }


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

    fieldType | signature.isOptional = "(Maybe " <> showTSType signature.type <> ")"
    fieldType = "(" <> showTSType signature.type <> ")"
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
showTSType (TypeReference { name}) = "(" <> (showEntityName name) {- <> " " <> (Array.intercalate " " $ map showTSType typeArguments) -} <> ")"
showTSType (FunctionType { typeParameters, parameters, returnType }) | Array.length typeParameters == 0 = (Array.intercalate " -> " $ map showTypeMemberAsFunctionType parameters) <> " -> " <> (showTSType returnType)
showTSType (FunctionType { typeParameters, parameters, returnType }) = "forall " <> (Array.intercalate " " $ map showTypeParameter typeParameters) <> ". " <> (Array.intercalate " -> " $ map showTypeMemberAsFunctionType parameters) <> " -> " <> (showTSType returnType)
showTSType (ArrayType t) = "(Array " <> (showTSType t) <> ")"
showTSType t = show t

showEntityName :: EntityName -> String
showEntityName (Identifier name) = name
showEntityName (QualifiedName { left, right }) = (showEntityName left) <> "." <> right

showPropertyName :: PropertyName -> String
showPropertyName (IdentifierName name) = name
showPropertyName (StringLiteral name) = name
showPropertyName (NumericLiteral name) = show (show name)


derive instance genericDeclarationSourceFile :: GR.Generic DeclarationSourceFile _
derive instance genericDeclarationModuleElements :: GR.Generic DeclarationModuleElements _
derive instance genericDeclarationElements :: GR.Generic DeclarationElements _
derive instance genericTypeParameter :: GR.Generic TypeParameter _
derive instance genericTSType :: GR.Generic TSType _
derive instance genericPropertyName :: GR.Generic PropertyName _
derive instance genericTypeMember :: GR.Generic TypeMember _
derive instance genericEntityName :: GR.Generic EntityName _
derive instance genericLiteralValue :: GR.Generic LiteralValue _

instance decodeDeclarationSourceFile :: Decode DeclarationSourceFile where decode = genericDecode defaultOptions
instance decodeDeclarationModule :: Decode DeclarationModuleElements where decode = genericDecode defaultOptions
instance decodeDeclarationElements :: Decode DeclarationElements where decode = genericDecode defaultOptions
instance decodeTypeParameter :: Decode TypeParameter where decode = genericDecode defaultOptions
instance decodeTSType :: Decode TSType where decode = fix \_ -> genericDecode defaultOptions
instance decodePropertyName :: Decode PropertyName where decode = genericDecode defaultOptions
instance decodeTypeMember :: Decode TypeMember where decode = fix \_ -> genericDecode defaultOptions
instance decodeEntityName :: Decode EntityName where decode = fix \_ -> genericDecode defaultOptions
instance decodeLiteralValue :: Decode LiteralValue where decode = fix \_ -> genericDecode defaultOptions

instance _showDeclarationSourceFile :: Show DeclarationSourceFile where show = genericShow
instance _showDeclarationModule :: Show DeclarationModuleElements where show = genericShow 
instance _showDeclarationElementsG :: Show DeclarationElements where show = genericShow
instance _showTypeParameter :: Show TypeParameter where show = genericShow
instance _showTSType :: Show TSType where show = fix \_ -> genericShow
instance _showPropertyName :: Show PropertyName where show = genericShow
instance _showTypeMember :: Show TypeMember where show = fix \_ -> genericShow
instance _showEntityName :: Show EntityName where show = fix \_ -> genericShow
instance _showLiteralValue :: Show LiteralValue where show = fix \_ -> genericShow


sourceFiles :: Effect (Array DeclarationSourceFile)
sourceFiles = do
  fs <- _sourceFiles
  traverse (liftEither <<< runExcept <<< decode) fs

liftEither :: ∀ e a. Show e => Either e a -> Effect a
liftEither = case _ of 
  Left e -> throw $ show e
  Right a -> pure a

liftMaybe :: ∀ a. String -> Maybe a -> Effect a
liftMaybe msg = case _ of 
  Nothing -> throw msg
  Just a -> pure a


foreign import _sourceFiles :: Effect (Array Foreign)