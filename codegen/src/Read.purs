module Codegen.Read where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep as GR
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Fold, Lens', Prism', Traversal', prism', toListOf, traversed)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Monoid.Endo (Endo)
import Data.String.Regex (Regex)
import Data.Symbol (SProxy(..))
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Foreign (Foreign)
import Foreign.Generic (class Decode, decode, defaultOptions, genericDecode)
import Foreign.Object (Object)
import Generic.Optic (_Ctor')

-- Types

data DeclarationSourceFile 
  = DeclarationSourceFile DeclarationSourceFileRec

type DeclarationSourceFileRec = { fileName :: String, elements :: Array DeclarationElements }

data DeclarationElements
  = AmbientDeclaration
  | ClassElement ClassElementRec
  | ExportAssignment (Maybe String)
  | ExportDeclaration
  | ExportDefaultDeclaration
  | FunctionElement FunctionElementRec
  | ImportDeclaration
  | ImportEqualsDeclaration
  | InterfaceDeclaration InterfaceDeclarationRec 
  | ModuleDeclaration ModuleDeclarationRec 
  | NamespaceExportDeclaration String
  | TypeAliasDeclaration TypeAliasDeclarationRec
  | VariableStatement (Array VariableDeclaration)

type ClassElementRec = { name :: Maybe String, fullyQualifiedName :: Maybe String }
type InterfaceDeclarationRec = { name :: String, fullyQualifiedName :: Maybe String, typeParameters :: Array TypeParameter, typeMembers :: Array TypeMember }
type FunctionElementRec = { name :: Maybe String, fullyQualifiedName :: Maybe String, typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
type ModuleDeclarationRec = { name :: String, body :: Maybe ModuleBody }
type TypeAliasDeclarationRec = { name :: String, fullyQualifiedName :: Maybe String, aliasName :: Maybe String, typeParameters :: Array TypeParameter, type :: TSType }

data VariableDeclaration = VariableDeclaration VariableDeclarationRec

type VariableDeclarationRec = { name :: String, fullyQualifiedName :: Maybe String, type :: TSType }

data ModuleBody 
  = NamespaceBodyDefinition NamespaceBody
  | JSDocNamespaceBody

data NamespaceBody
  = ModuleBlock (Array DeclarationElements)
  | NamespaceDeclaration NamespaceDeclarationRec

type NamespaceDeclarationRec = { name :: String, body :: NamespaceBody }

data TypeParameter = TypeParameter String

data PropertyName 
  = IdentifierName String
  | StringLiteral String
  | NumericLiteral Number

data TypeMember
  = PropertySignature PropertySignatureRec 
  | CallSignature FunctionLikeRec
  | ConstructSignature FunctionLikeRec
  | MethodSignature FunctionLikeRec
  | IndexSignature IndexSignatureRec

type PropertySignatureRec = { name :: Maybe PropertyName, isOptional :: Boolean, type :: TSType }
type FunctionLikeRec = { name :: Maybe PropertyName, isOptional :: Boolean, typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
type IndexSignatureRec = { name :: Maybe PropertyName, isOptional :: Boolean, typeParameters :: Array TypeParameter, parameters :: Array TypeMember }

data LiteralValue = LiteralStringValue String | LiteralNumericValue String | LiteralBigIntValue String | LiteralBooleanValue Boolean 

data TSType 
  = AnyType
  | ArrayType TSType
  | BigIntType
  | BooleanType
  | ConditionalType ConditionalTypeRec
  | ConstructorType ConstructorTypeRec
  | FalseType
  | FunctionType FunctionTypeRec
  | IndexAccessType IndexAccessTypeRec
  | InferType TypeParameter
  | IntersectionType (Array TSType)
  | LiteralType LiteralValue
  | MappedType MappedTypeRec
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
  | TypeReference TypeReferenceRec
  | UndefinedType
  | UnionType (Array TSType)
  | UnknownType 
  | VoidType

type ConditionalTypeRec = { checkType :: TSType, extendsType :: TSType, trueType :: TSType, falseType :: TSType }
type ConstructorTypeRec = { typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
type FunctionTypeRec = { typeParameters :: Array TypeParameter, parameters :: Array TypeMember, returnType :: TSType }
type IndexAccessTypeRec = { indexType :: TSType, objectType :: TSType }
type MappedTypeRec = { isOptional :: Boolean, type :: TSType, typeParameter :: Maybe TypeParameter }
type TypeReferenceRec = { name :: EntityName, fullyQualifiedName :: Maybe String, typeArguments :: Array TSType, aliasName :: Maybe EntityName, aliasTypeArguments :: Maybe (Array TSType) }

data EntityName 
  = Identifier String
  | QualifiedName QualifiedNameRec

type QualifiedNameRec = { left :: EntityName, right :: String }

-- Helpers

interfaces :: DeclarationSourceFile -> (Array InterfaceDeclarationRec)
interfaces declarationSourceFile = toArrayOf _interfaces declarationSourceFile

interfaceByName :: DeclarationSourceFile -> String -> Maybe InterfaceDeclarationRec
interfaceByName file interfaceName = Array.find (\{ name } -> name == interfaceName) (interfaces file)

typescript :: String -> Regex -> Effect { sources :: (Array DeclarationSourceFile), types :: Object Foreign }
typescript path regex = do
  rec     <- _typescript path regex
  sources <- sourceFiles rec.sources
  pure { sources, types: rec.types }

sourceFiles :: Array Foreign -> Effect (Array DeclarationSourceFile)
sourceFiles sources = do
  traverse convert sources 
  where
    convert :: Foreign -> Effect DeclarationSourceFile
    convert f = do
      currentPath <- _fileName f
      log $ "Attempting to convert TS AST to PS AST " <> currentPath
      decl <- liftEither $ runExcept $ decode f
      logConverted decl
    logConverted :: DeclarationSourceFile -> Effect DeclarationSourceFile
    logConverted d@(DeclarationSourceFile { fileName }) = do
      log ("Successfully Converted " <> fileName)
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

toArrayOf :: forall s t a b. Fold (Endo (->) (List a)) s t a b -> s -> Array a
toArrayOf p = Array.fromFoldable <<< toListOf p

_interfaces ::  Traversal' DeclarationSourceFile InterfaceDeclarationRec
_interfaces = 
  _DeclarationSourceFile <<<
  _elements <<<
  traversed <<<
  _InterfaceDeclaration

_AmbientDeclaration :: Prism' DeclarationElements Unit
_AmbientDeclaration = _Ctor' (SProxy :: SProxy "AmbientDeclaration")

_ExportAssignment :: Prism' DeclarationElements (Maybe String)
_ExportAssignment = _Ctor' (SProxy :: SProxy "ExportAssignment")

_ExportDeclaration :: Prism' DeclarationElements Unit
_ExportDeclaration = _Ctor' (SProxy :: SProxy "ExportDeclaration")

_ExportDefaultDeclaration :: Prism' DeclarationElements Unit
_ExportDefaultDeclaration = _Ctor' (SProxy :: SProxy "ExportDefaultDeclaration")

_ImportDeclaration :: Prism' DeclarationElements Unit
_ImportDeclaration = _Ctor' (SProxy :: SProxy "ImportDeclaration")

_ImportEqualsDeclaration :: Prism' DeclarationElements Unit
_ImportEqualsDeclaration = _Ctor' (SProxy :: SProxy "ImportEqualsDeclaration")

_NamespaceExportDeclaration :: Prism' DeclarationElements String 
_NamespaceExportDeclaration = _Ctor' (SProxy :: SProxy "NamespaceExportDeclaration")

_InterfaceDeclaration :: Prism' DeclarationElements InterfaceDeclarationRec
_InterfaceDeclaration = _Ctor' (SProxy :: SProxy "InterfaceDeclaration")  

_ClassElement :: Prism' DeclarationElements ClassElementRec
_ClassElement = _Ctor' (SProxy :: SProxy "ClassElement")  

_FunctionElement :: Prism' DeclarationElements FunctionElementRec
_FunctionElement = _Ctor' (SProxy :: SProxy "FunctionElement")  

_ModuleDeclaration :: Prism' DeclarationElements ModuleDeclarationRec
_ModuleDeclaration = _Ctor' (SProxy :: SProxy "ModuleDeclaration")  

_TypeAliasDeclaration :: Prism' DeclarationElements TypeAliasDeclarationRec
_TypeAliasDeclaration = _Ctor' (SProxy :: SProxy "TypeAliasDeclaration")  

_VariableStatement :: Prism' DeclarationElements (Array VariableDeclaration)
_VariableStatement = _Ctor' (SProxy :: SProxy "VariableStatement")

_VariableDeclaration :: Prism' VariableDeclaration VariableDeclarationRec
_VariableDeclaration = _Ctor' (SProxy :: SProxy "VariableDeclaration")

_NamespaceBodyDefinition :: Prism' ModuleBody NamespaceBody
_NamespaceBodyDefinition = _Ctor' (SProxy :: SProxy "NamespaceBodyDefinition")

_JSDocNamespaceBody :: Prism' ModuleBody Unit 
_JSDocNamespaceBody = _Ctor' (SProxy :: SProxy "JSDocNamespaceBody")

_ModuleBlock :: Prism' NamespaceBody (Array DeclarationElements)
_ModuleBlock = _Ctor' (SProxy :: SProxy "ModuleBlock")

_NamespaceDeclaration :: Prism' NamespaceBody NamespaceDeclarationRec
_NamespaceDeclaration = _Ctor' (SProxy :: SProxy "NamespaceDeclaration")

_TypeParameter:: Prism' TypeParameter String 
_TypeParameter = _Ctor' (SProxy :: SProxy "TypeParameter")

_IdentifierName :: Prism' PropertyName String 
_IdentifierName = _Ctor' (SProxy :: SProxy "IdentifierName")

_StringLiteral :: Prism' PropertyName String 
_StringLiteral = _Ctor' (SProxy :: SProxy "StringLiteral")

_NumericLiteral :: Prism' PropertyName Number 
_NumericLiteral = _Ctor' (SProxy :: SProxy "NumericLiteral")

_PropertySignature :: Prism' TypeMember PropertySignatureRec
_PropertySignature = _Ctor' (SProxy :: SProxy "PropertySignature")

_CallSignature :: Prism' TypeMember FunctionLikeRec 
_CallSignature = _Ctor' (SProxy :: SProxy "CallSignature")

_MethodSignature :: Prism' TypeMember FunctionLikeRec 
_MethodSignature = _Ctor' (SProxy :: SProxy "MethodSignature")

_ConstructSignature :: Prism' TypeMember FunctionLikeRec 
_ConstructSignature = _Ctor' (SProxy :: SProxy "ConstructSignature")

_IndexSignature :: Prism' TypeMember IndexSignatureRec
_IndexSignature = _Ctor' (SProxy :: SProxy "IndexSignature")

_LiteralStringValue :: Prism' LiteralValue String 
_LiteralStringValue = _Ctor' (SProxy :: SProxy "LiteralStringValue")

_LiteralNumericValue :: Prism' LiteralValue String
_LiteralNumericValue = _Ctor' (SProxy :: SProxy "LiteralNumericValue")

_LiteralBigIntValue :: Prism' LiteralValue String
_LiteralBigIntValue = _Ctor' (SProxy :: SProxy "LiteralBigIntValue")

_LiteralBooleanValue :: Prism' LiteralValue Boolean 
_LiteralBooleanValue = _Ctor' (SProxy :: SProxy "LiteralBooleanValue")

_ArrayType :: Prism' TSType TSType
_ArrayType  = _Ctor' (SProxy :: SProxy "ArrayType")

_InferType :: Prism' TSType TypeParameter
_InferType = _Ctor' (SProxy :: SProxy "InferType")

_IntersectionType :: Prism' TSType (Array TSType)
_IntersectionType = _Ctor' (SProxy :: SProxy "IntersectionType")

_LiteralType :: Prism' TSType LiteralValue
_LiteralType = _Ctor' (SProxy :: SProxy "LiteralType")

_ParenthesizedType :: Prism' TSType TSType
_ParenthesizedType = _Ctor' (SProxy :: SProxy "ParenthesizedType")

_TupleType :: Prism' TSType (Array TSType)
_TupleType = _Ctor' (SProxy :: SProxy "TupleType")

_TypeLiteral :: Prism' TSType (Array TypeMember)
_TypeLiteral = _Ctor' (SProxy :: SProxy "TypeLiteral")

_UnionType :: Prism' TSType (Array TSType)
_UnionType = _Ctor' (SProxy :: SProxy "UnionType")

_AnyType :: Prism' TSType Unit
_AnyType = _Ctor' (SProxy :: SProxy "AnyType")

_BigIntType :: Prism' TSType Unit
_BigIntType = _Ctor' (SProxy :: SProxy "BigIntType")

_BooleanType :: Prism' TSType Unit
_BooleanType = _Ctor' (SProxy :: SProxy "BooleanType")

_FalseType :: Prism' TSType Unit
_FalseType = _Ctor' (SProxy :: SProxy "FalseType")

_NeverType  :: Prism' TSType Unit
_NeverType = _Ctor' (SProxy :: SProxy "NeverType")

_NullType :: Prism' TSType Unit
_NullType = _Ctor' (SProxy :: SProxy "NullType")

_NumberType :: Prism' TSType Unit
_NumberType = _Ctor' (SProxy :: SProxy "NumberType")

_ObjectType :: Prism' TSType Unit
_ObjectType = _Ctor' (SProxy :: SProxy "ObjectType")

_StringType :: Prism' TSType Unit
_StringType = _Ctor' (SProxy :: SProxy "StringType")

_SymbolType :: Prism' TSType Unit
_SymbolType = _Ctor' (SProxy :: SProxy "SymbolType")

_ThisType :: Prism' TSType Unit
_ThisType = _Ctor' (SProxy :: SProxy "ThisType")

_TrueType :: Prism' TSType Unit
_TrueType = _Ctor' (SProxy :: SProxy "TrueType")

_TypeOperator :: Prism' TSType Unit
_TypeOperator = _Ctor' (SProxy :: SProxy "TypeOperator")

_TypeQuery :: Prism' TSType Unit
_TypeQuery = _Ctor' (SProxy :: SProxy "TypeQuery")

_TypeReference :: Prism' TSType TypeReferenceRec 
_TypeReference = _Ctor' (SProxy :: SProxy "TypeReference")

_UndefinedType :: Prism' TSType Unit
_UndefinedType = _Ctor' (SProxy :: SProxy "UndefinedType")

_UnknownType  :: Prism' TSType Unit
_UnknownType = _Ctor' (SProxy :: SProxy "UnknownType")

_VoidType :: Prism' TSType Unit
_VoidType = _Ctor' (SProxy :: SProxy "VoidType")

_body :: ∀ a r. Lens' { body :: a | r } a
_body = prop (SProxy :: SProxy "body")

_type :: ∀ a r. Lens' { type :: a | r } a
_type = prop (SProxy :: SProxy "type")

_returnType :: ∀ a r. Lens' { returnType :: a | r } a
_returnType = prop (SProxy :: SProxy "returnType")

_name :: ∀ a r. Lens' { name :: a | r } a
_name = prop (SProxy :: SProxy "name")

_names :: ∀ a r. Lens' { names :: a | r } a
_names = prop (SProxy :: SProxy "names")

_propertyName :: ∀ a r. Lens' { propertyName :: a | r } a
_propertyName = prop (SProxy :: SProxy "propertyName")



_checkType :: ∀ a r. Lens' { checkType :: a | r } a
_checkType = prop (SProxy :: SProxy "checkType")

_falseType :: ∀ a r. Lens' { falseType :: a | r } a
_falseType = prop (SProxy :: SProxy "falseType")

_trueType :: ∀ a r. Lens' { trueType :: a | r } a
_trueType = prop (SProxy :: SProxy "trueType")

_extendsType :: ∀ a r. Lens' { extendsType :: a | r } a
_extendsType = prop (SProxy :: SProxy "extendsType")

_typeParameters :: ∀ a r. Lens' { typeParameters :: a | r } a
_typeParameters = prop (SProxy :: SProxy "typeParameters")

_typeMembers :: ∀ a r. Lens' { typeMembers :: a | r } a
_typeMembers = prop (SProxy :: SProxy "typeMembers")

_parameters :: ∀ a r. Lens' { parameters :: a | r } a
_parameters = prop (SProxy :: SProxy "parameters")

_isOptional :: ∀ a r. Lens' { isOptional :: a | r } a
_isOptional = prop (SProxy :: SProxy "isOptional")

_objectType :: ∀ a r. Lens' { objectType :: a | r } a
_objectType = prop (SProxy :: SProxy "objectType")

_indexType :: ∀ a r. Lens' { indexType :: a | r } a
_indexType = prop (SProxy :: SProxy "indexType")

_aliasTypeArguments :: ∀ a r. Lens' { aliasTypeArguments :: a | r } a
_aliasTypeArguments = prop (SProxy :: SProxy "aliasTypeArguments")

_aliasName :: ∀ a r. Lens' { aliasName :: a | r } a
_aliasName = prop (SProxy :: SProxy "aliasName")

_typeArguments :: ∀ a r. Lens' { typeArguments :: a | r } a
_typeArguments = prop (SProxy :: SProxy "typeArguments")

_fullyQualifiedName :: ∀ a r. Lens' { fullyQualifiedName :: a | r } a
_fullyQualifiedName = prop (SProxy :: SProxy "fullyQualifiedName")

_left :: ∀ a r. Lens' { left :: a | r } a
_left = prop (SProxy :: SProxy "left")

_right :: ∀ a r. Lens' { right :: a | r } a
_right = prop (SProxy :: SProxy "right")

_Identifier :: Prism' EntityName String
_Identifier = _Ctor' (SProxy :: SProxy "Identifier")

_QualifiedName :: Prism' EntityName QualifiedNameRec
_QualifiedName = _Ctor' (SProxy :: SProxy "QualifiedName")

_isDefault :: ∀ a r. Lens' { isDefault :: a | r } a
_isDefault = prop (SProxy :: SProxy "isDefault")


-- FFI

foreign import _typescript :: String -> Regex -> Effect { sources :: (Array Foreign), types :: Object Foreign }
foreign import _fileName :: Foreign -> Effect String

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

instance _eqDeclarationSourceFile :: Eq DeclarationSourceFile where eq = genericEq
instance _eqModuleBody :: Eq ModuleBody where eq = genericEq 
instance _eqNamespaceBody :: Eq NamespaceBody where eq = fix \_ -> genericEq 
instance _eqDeclarationElements :: Eq DeclarationElements where eq = fix \_ -> genericEq
instance _eqVariableDeclaration :: Eq VariableDeclaration where eq = fix \_ -> genericEq
instance _eqTypeParameter :: Eq TypeParameter where eq = genericEq
instance _eqTSType :: Eq TSType where eq = fix \_ -> genericEq
instance _eqPropertyName :: Eq PropertyName where eq = genericEq
instance _eqTypeMember :: Eq TypeMember where eq = fix \_ -> genericEq
instance _eqEntityName :: Eq EntityName where eq = fix \_ -> genericEq
instance _eqLiteralValue :: Eq LiteralValue where eq = fix \_ -> genericEq
