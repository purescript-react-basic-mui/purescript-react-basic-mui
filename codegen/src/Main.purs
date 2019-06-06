module Codegen.Main where

import Prelude

import Control.Lazy (fix)
import Control.Monad.Except (runExcept)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep as GR
import Data.Maybe (isJust)
import Data.String (Pattern(..))
import Data.String as String
import Data.Traversable (traverse)
import Effect (Effect, foreachE)
import Effect.Console (log)
import Effect.Exception (throw)
import Foreign (Foreign)
import Foreign.Class (class Decode, decode)
import Foreign.Generic (defaultOptions, genericDecode)

data TSType
  = AnonymousObject { members :: Array Member }
  | AnyType
  | BooleanType
  | ExceptionType { error :: String, type :: String, flags :: Number } 
  -- | FunctionType { parameters :: Array Member, returnType :: Undefinable TSType }
  | FunctionType { parameters :: Array Member }
  | InterfaceReference { name :: String }
  | NullType
  | NumberType
  | NumericLiteralType { value :: String }
  | StringLiteralType { value :: String }
  | StringType
  | TupleType { types :: Array TSType }
  | TypeAlias { typeReference :: String, typeParams :: Array TSType, type :: TSType }
  | TypeParam { name :: String }
  | TypeReference { name :: String, typeParams :: Array TSType, flags :: Number, objFlags :: Number }
  | UnionType { types :: Array TSType } 
  | Unknown { name :: String, flags :: Number }
  | UnknownObject { flags :: Number }
  | VoidType

data Member = Member { name :: String, type :: TSType, optional :: Boolean }
data Interface = Interface { fileName :: String, name :: String, members :: Array Member }

derive instance genericTSType :: GR.Generic TSType _
derive instance genericMember :: GR.Generic Member _
derive instance genericInterface :: GR.Generic Interface _

instance decodeTSType :: Decode TSType where decode = fix \_ -> genericDecode defaultOptions
instance decodeMember :: Decode Member where decode = genericDecode defaultOptions
instance decodeInterface :: Decode Interface where decode = genericDecode defaultOptions

getInterfaces :: Effect (Array Interface)
getInterfaces = do
  fs <- _interfaces
  traverse (liftEither <<< runExcept <<< decode) fs

materialInterfaces :: Effect (Array Interface)
materialInterfaces = do
  interfaces <- getInterfaces
  pure $ Array.filter (\(Interface interface) ->
    isJust $ String.indexOf (Pattern "@material-ui") interface.fileName) interfaces

liftEither :: ∀ e a. Show e => Either e a -> Effect a
liftEither = case _ of 
  Left e -> throw $ show e
  Right a -> pure a

main :: Effect Unit
main = do
  interfaces <- materialInterfaces
  foreachE interfaces \(Interface interface) -> log interface.name

foreign import _interfaces :: Effect (Array Foreign)
