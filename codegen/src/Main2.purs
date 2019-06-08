module Codegen.Main2 where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep as GR
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), isJust)
import Data.String.Regex as String
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Foreign (Foreign)
import Foreign.Generic (class Decode, decode, defaultOptions, genericDecode)

data DeclarationSourceFile 
  = DeclarationSourceFile { fileName :: String, declarationModuleElements :: Array DeclarationModuleElements }

data DeclarationModuleElements
  = DeclarationElement DeclarationElements
  | ImportDeclaration
  | ImportEqualsDeclaration
  | ExportDeclaration
  | ExportDefaultDeclaration
  | ExportAssignment

data DeclarationElements
  = InterfaceDeclaration { name :: String, typeParameters :: Array TypeParameter, typeMembers :: Array TypeMember }
  | TypeAliasDeclaration { name :: String, aliasName :: Maybe String, type :: TSType }
  | AmbientDeclaration

data TypeParameter = TypeParameter String
data PropertyName 
  = IdentifierName String
  | StringLiteral String
  | NumericLiteral Number

data TypeMember
  = PropertySignature { name :: Maybe PropertyName, type :: TSType, isOptional :: Boolean }

data LiteralValue = LiteralStringValue String | LiteralNumericValue String | LiteralBigInt String

data TSType 
  = ThisType
  | BooleanType
  | StringType
  | NumberType
  | BigIntType
  | ObjectType
  | VoidType
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
  | AnonymousObjectType (Array TypeMember)
  | TypeReference { name :: EntityName, typeParameters :: Array TSType }
  | ParenthesizedType TSType
  | ConstructorType { typeParameters :: Array TSType, parameters :: Array TypeMember, returnType :: TSType }
  | FunctionType { typeParameters :: Array TSType, parameters :: Array TypeMember, returnType :: TSType }
  | ClassType { name :: Maybe String, typeParameters :: Array TypeParameter, typeMembers :: Array TypeMember }
  | InterfaceType { name :: Maybe String, typeParameters :: Array TypeParameter, typeMembers :: Array TypeMember }
  | LiteralType LiteralValue
  | IndexAccessType { indexType :: TSType, objectType :: TSType }
  | ConditionalType { checkType :: TSType, extendsType :: TSType, trueType :: TSType, falseType :: TSType }

data EntityName 
  = Identifier String
  | QualifiedName EntityName String


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
instance decodeTypeMember :: Decode TypeMember where decode = genericDecode defaultOptions
instance decodeEntityName :: Decode EntityName where decode = fix \_ -> genericDecode defaultOptions
instance decodeLiteralValue :: Decode LiteralValue where decode = fix \_ -> genericDecode defaultOptions

instance showDeclarationSourceFile :: Show DeclarationSourceFile where show = genericShow
instance showDeclarationModule :: Show DeclarationModuleElements where show = genericShow 
instance showDeclarationElements :: Show DeclarationElements where show = genericShow
instance showTypeParameter :: Show TypeParameter where show = genericShow
instance showTSType :: Show TSType where show = fix \_ -> genericShow
instance showPropertyName :: Show PropertyName where show = genericShow
instance showTypeMember :: Show TypeMember where show = fix \_ -> genericShow
instance showEntityName :: Show EntityName where show = fix \_ -> genericShow
instance showLiteralValue :: Show LiteralValue where show = fix \_ -> genericShow

main :: Effect Unit
main = do
  sources       <- sourceFiles
  avatarRegex   <- liftEither $ String.regex "\\/Avatar.d.ts$" RegexFlags.noFlags
  cardRegex     <- liftEither $ String.regex "\\/Card.d.ts$" RegexFlags.noFlags
  avatar        <- liftMaybe "Couldn't find Avatar" $ Array.head $ Array.filter (\(DeclarationSourceFile { fileName }) -> isJust $ String.match avatarRegex fileName) sources
  card          <- liftMaybe "Couldn't find Card" $ Array.head $ Array.filter (\(DeclarationSourceFile { fileName }) -> isJust $ String.match cardRegex fileName) sources
  log $ show avatar
  log "\n\n"
  log $ show card


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