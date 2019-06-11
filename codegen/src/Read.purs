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
import Data.String.Regex (Regex)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Foreign (Foreign)
import Foreign.Generic (class Decode, decode, defaultOptions, genericDecode)


-- Types

data DeclarationSourceFile 
  = DeclarationSourceFile DeclarationSourceFileRec

type DeclarationSourceFileRec = { fileName :: String, elements :: Array DeclarationElements }

data DeclarationElements
  = AmbientDeclaration
  | ClassElement { name :: String }
  | ExportAssignment
  | ExportDeclaration
  | ExportDefaultDeclaration
  | FunctionElement { name :: Maybe PropertyName, typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
  | ImportDeclaration
  | ImportEqualsDeclaration
  | InterfaceDeclaration InterfaceDeclarationRec 
  | ModuleDeclaration { name :: String, body :: Maybe ModuleBody }
  | NamepaceExportDeclaration String
  | TypeAliasDeclaration { name :: String, aliasName :: Maybe String, type :: TSType }
  | VariableStatement (Array VariableDeclaration)

type InterfaceDeclarationRec = { name :: String, typeParameters :: Array TypeParameter, typeMembers :: Array TypeMember }

data VariableDeclaration = VariableDeclaration { name :: String, type :: TSType }

data ModuleBody 
  = NamespaceBodyDefinition NamespaceBody
  | JSDocNamespaceBody

data NamespaceBody
  = ModuleBlock (Array DeclarationElements)
  | NamespaceDeclaration { name :: String, body :: NamespaceBody }

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
  = AnyType
  | ArrayType TSType
  | BigIntType
  | BooleanType
  | ClassType { name :: Maybe String, typeParameters :: Array TypeParameter, typeMembers :: Array TypeMember }
  | ConditionalType { checkType :: TSType, extendsType :: TSType, trueType :: TSType, falseType :: TSType }
  | ConstructorType { typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
  | FalseType
  | FunctionType { typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
  | IndexAccessType { indexType :: TSType, objectType :: TSType }
  | InferType TypeParameter
  | InterfaceType { name :: Maybe String, typeParameters :: Array TypeParameter, typeMembers :: Array TypeMember }
  | IntersectionType (Array TSType)
  | LiteralType LiteralValue
  | MappedType { isOptional :: Boolean, type :: TSType, typeParameter :: Maybe TypeParameter }
  | NeverType 
  | NullType
  | NumberType
  | ObjectType
  | ParenthesizedType TSType
  | StringType
  | SymbolType
  | ThisType
  | TrueType
  | TupleType (Array TSType)
  | TypeLiteral (Array TypeMember)
  | TypeOperator
  | TypeQuery
  | TypeReference { name :: EntityName, typeArguments :: Array TSType, aliasName :: Maybe EntityName, aliasTypeArguments :: Maybe (Array TSType) }
  | UndefinedType
  | UnionType (Array TSType)
  | UnknownType 
  | VoidType

data EntityName 
  = Identifier String
  | QualifiedName { left :: EntityName, right :: String }

-- Helpers

interfaces :: DeclarationSourceFile -> (Array InterfaceDeclarationRec)
interfaces declarationSourceFile = toArrayOf _interfaces declarationSourceFile

interfaceByName :: DeclarationSourceFile -> String -> Maybe InterfaceDeclarationRec
interfaceByName file interfaceName = Array.find (\{ name } -> name == interfaceName) (interfaces file)

sourceFiles :: Regex -> Effect (Array DeclarationSourceFile)
sourceFiles regex = do
  fs    <- _sourceFiles regex
  traverse convert fs
  where
    convert :: Foreign -> Effect DeclarationSourceFile
    convert f = do
      let _ = spy "f" f
      decl <- liftEither $ runExcept $ decode f
      logConverted decl
    logConverted :: DeclarationSourceFile -> Effect DeclarationSourceFile
    logConverted d@(DeclarationSourceFile { fileName }) = do
      log ("Converted " <> fileName)
      pure d

isOptional :: TypeMember -> Boolean
isOptional (PropertySignature rec) = rec.isOptional
isOptional (CallSignature rec) = rec.isOptional
isOptional (ConstructSignature rec) = rec.isOptional
isOptional (MethodSignature rec) = rec.isOptional
isOptional (IndexSignature rec) = rec.isOptional


liftEither :: ∀ e a. Show e => Either e a -> Effect a
liftEither = case _ of 
  Left e -> throw $ show e
  Right a -> pure a

liftMaybe :: ∀ a. String -> Maybe a -> Effect a
liftMaybe msg = case _ of 
  Nothing -> throw msg
  Just a -> pure a

-- Lenses

_DeclarationSourceFile :: Prism' DeclarationSourceFile DeclarationSourceFileRec
_DeclarationSourceFile = prism' DeclarationSourceFile case _ of 
  DeclarationSourceFile rec -> Just rec

_elements :: ∀ r. Lens' { elements :: Array DeclarationElements | r } (Array DeclarationElements)
_elements = prop (SProxy :: SProxy "elements")

_InterfaceDeclaration :: Prism' DeclarationElements InterfaceDeclarationRec
_InterfaceDeclaration = prism' InterfaceDeclaration case _ of 
  InterfaceDeclaration rec -> Just rec
  _ -> Nothing

toArrayOf :: forall s t a b. Fold (Endo (->) (List a)) s t a b -> s -> Array a
toArrayOf p = Array.fromFoldable <<< toListOf p

_interfaces ::  Traversal' DeclarationSourceFile InterfaceDeclarationRec
_interfaces = 
  _DeclarationSourceFile <<<
  _elements <<<
  traversed <<<
  _InterfaceDeclaration


-- FFI

foreign import _sourceFiles :: Regex -> Effect (Array Foreign)

-- Type class instances

derive instance genericDeclarationSourceFile :: GR.Generic DeclarationSourceFile _
derive instance genericModuleBody :: GR.Generic ModuleBody _
derive instance genericNamespaceBody :: GR.Generic NamespaceBody _
derive instance genericDeclarationElements :: GR.Generic DeclarationElements _
derive instance genericVariableDeclaration :: GR.Generic VariableDeclaration _
derive instance genericTypeParameter :: GR.Generic TypeParameter _
derive instance genericTSType :: GR.Generic TSType _
derive instance genericPropertyName :: GR.Generic PropertyName _
derive instance genericTypeMember :: GR.Generic TypeMember _
derive instance genericEntityName :: GR.Generic EntityName _
derive instance genericLiteralValue :: GR.Generic LiteralValue _

instance decodeDeclarationSourceFile :: Decode DeclarationSourceFile where decode = genericDecode defaultOptions
instance decodeModuleBody :: Decode ModuleBody where decode = genericDecode defaultOptions
instance decodeNamespaceBody :: Decode NamespaceBody where decode = fix \_ -> genericDecode defaultOptions
instance decodeDeclarationElements :: Decode DeclarationElements where decode = fix \_ -> genericDecode defaultOptions
instance decodeVariableDeclaration :: Decode VariableDeclaration where decode = fix \_ -> genericDecode defaultOptions
instance decodeTypeParameter :: Decode TypeParameter where decode = genericDecode defaultOptions
instance decodeTSType :: Decode TSType where decode = fix \_ -> genericDecode defaultOptions
instance decodePropertyName :: Decode PropertyName where decode = genericDecode defaultOptions
instance decodeTypeMember :: Decode TypeMember where decode = fix \_ -> genericDecode defaultOptions
instance decodeEntityName :: Decode EntityName where decode = fix \_ -> genericDecode defaultOptions
instance decodeLiteralValue :: Decode LiteralValue where decode = fix \_ -> genericDecode defaultOptions

instance _showDeclarationSourceFile :: Show DeclarationSourceFile where show = genericShow
instance _showModuleBody :: Show ModuleBody where show = genericShow 
instance _showNamespaceBody :: Show NamespaceBody where show = fix \_ -> genericShow 
instance _showDeclarationElements :: Show DeclarationElements where show = fix \_ -> genericShow
instance _showVariableDeclaration :: Show VariableDeclaration where show = fix \_ -> genericShow
instance _showTypeParameter :: Show TypeParameter where show = genericShow
instance _showTSType :: Show TSType where show = fix \_ -> genericShow
instance _showPropertyName :: Show PropertyName where show = genericShow
instance _showTypeMember :: Show TypeMember where show = fix \_ -> genericShow
instance _showEntityName :: Show EntityName where show = fix \_ -> genericShow
instance _showLiteralValue :: Show LiteralValue where show = fix \_ -> genericShow
