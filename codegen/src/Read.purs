module Codegen.Read where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep as GR
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (Fold, Lens', Prism', preview, prism', toListOf, traversed)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid.Endo (Endo)
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


interfaces :: DeclarationSourceFile -> (Array InterfaceDeclarationRec)
interfaces declarationSourceFile = fromMaybe [] do
  let maybeArray = preview (_DeclarationSourceFile <<< _declarationModuleElements) declarationSourceFile 
  maybeArray <#> \array -> Array.mapMaybe (preview (_DeclarationElement <<< _InterfaceDeclaration)) array

interfaces2 :: DeclarationSourceFile -> (Array InterfaceDeclarationRec)
interfaces2 declarationSourceFile = 
  toArrayOf (
    _DeclarationSourceFile <<<
    _declarationModuleElements <<<
    traversed <<<
    _DeclarationElement <<<
    _InterfaceDeclaration
  ) declarationSourceFile


interfaceByName :: DeclarationSourceFile -> String -> Maybe InterfaceDeclarationRec
interfaceByName file interfaceName = Array.find (\{ name } -> name == interfaceName) (interfaces file)

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


showDeclarationElements :: DeclarationElements -> String
showDeclarationElements (InterfaceDeclaration { name, typeParameters, typeMembers }) = name
showDeclarationElements element = show element


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
instance showDeclarationElementsG :: Show DeclarationElements where show = genericShow
instance showTypeParameter :: Show TypeParameter where show = genericShow
instance showTSType :: Show TSType where show = fix \_ -> genericShow
instance showPropertyName :: Show PropertyName where show = genericShow
instance showTypeMember :: Show TypeMember where show = fix \_ -> genericShow
instance showEntityName :: Show EntityName where show = fix \_ -> genericShow
instance showLiteralValue :: Show LiteralValue where show = fix \_ -> genericShow


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