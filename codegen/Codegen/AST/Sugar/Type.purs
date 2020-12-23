module Codegen.AST.Sugar.Type where

import Prelude

import Codegen.AST.Types (Fields, Ident(..), ModuleName(..), QualifiedName, Row, RowF(..), RowLabel, Type, TypeF(..), TypeName)
import Data.Array (fromFoldable) as Array
import Data.Array (unsnoc)
import Data.Foldable (intercalate)
import Data.Functor.Mu (roll)
import Data.List (List(..)) as List
import Data.List (List)
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.String (Pattern(..), split)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Prim.RowList (class RowToList)
import Record.Extra (class MapRecord, mapRecord)

app :: Type -> Array Type -> Type
app n = roll <<< TypeApp n

arr :: Type -> Type -> Type
arr f = roll <<< TypeArr f

array :: Type -> Type
array = roll <<< TypeArray

boolean :: Type
boolean = roll TypeBoolean

constructor :: String -> Type
constructor = roll <<< TypeConstructor <<< name

constrained :: String -> Array Type -> Type -> Type
constrained s params = roll <<< TypeConstrained { className: name' s, params }

-- -- | Turns given record of strings into a record of type variables
-- -- | which are passed to type building function.
-- -- | Finally wrap its result with `ForAll`.
-- -- |
-- -- | ```
-- -- | signature = forAll { g: "given", r: "required"} \{ g, r } ->
-- -- |  let
-- -- |    fun = arr (recordApply g) (constructor "ResultType")
-- -- |  in
-- -- |    constrained "Prim.Row.Union" [ g, r, constructor "FinalRow" ] fun
-- -- |
-- -- | ```
-- -- | Gives us:
-- -- |
-- -- | forall required given. Prim.Row.Union given required FinalRow => Record given -> ResultType
-- -- |
forAll ::
  forall idents il names nl vars.
  HFoldl (List Ident -> Ident -> List Ident) (List Ident) (Record idents) (List Ident) =>
  RowToList names nl =>
  RowToList idents il =>
  MapRecord nl names String Ident () idents =>
  MapRecord il idents Ident Type () vars =>
  Record names ->
  (Record vars -> Type) ->
  Type
forAll names cont =
  -- | It is horrible but this don't want to work so I've copied
  -- | the whole `forAllWith` below :-(
  -- forAllWith [] names cont
  let
    varsRecord = mapRecord Ident names

    toList = hfoldl (flip List.Cons :: List Ident -> Ident -> List Ident) (List.Nil :: List Ident)

    varsRecord' = mapRecord (roll <<< TypeVar) varsRecord

    idents' = Array.fromFoldable (toList varsRecord)
  in
    roll (TypeForall idents' (cont varsRecord'))

forAllWith ::
  forall idents il names nl vars.
  HFoldl (List Ident -> Ident -> List Ident) (List Ident) (Record idents) (List Ident) =>
  RowToList names nl =>
  RowToList idents il =>
  MapRecord nl names String Ident () idents =>
  MapRecord il idents Ident Type () vars =>
  Array Ident ->
  Record names ->
  (Record vars -> Type) ->
  Type
forAllWith extraIdents names cont =
  let
    varsRecord = mapRecord Ident names

    toList = hfoldl (flip List.Cons :: List Ident -> Ident -> List Ident) (List.Nil :: List Ident)

    varsRecord' = mapRecord (roll <<< TypeVar) varsRecord

    idents' = extraIdents <> Array.fromFoldable (toList varsRecord)
  in
    roll (TypeForall idents' (cont varsRecord'))

forAll' :: String -> (Type -> Type) -> Type
forAll' n cont =
  let
    ident = Ident n

    v = roll $ TypeVar $ ident
  in
    roll (TypeForall [ ident ] (cont v))

int :: Type
int = roll TypeInt

name :: String -> QualifiedName TypeName
name = name'

name' :: forall n. Newtype n String => String -> QualifiedName n
name' n = qn n
  where
  qn =
    split (Pattern ".") >>> unsnoc
      >>> case _ of
          Just { init, last } ->
            { name: wrap last
            , moduleName:
              case init of
                [] -> Nothing
                otherwise -> Just $ ModuleName $ intercalate "." init
            }
          Nothing -> { name: wrap n, moduleName: Nothing }

number :: Type
number = roll TypeNumber

record :: Row -> Type
record = roll <<< TypeRecord

recordLiteral :: Type -> Type
recordLiteral tail = record $ Row { labels: mempty, tail: Just tail }

recordLiteral' :: Fields Type -> Type -> Type
recordLiteral' fields tail = record $ Row { labels: fields, tail: Just tail }

recordApply :: Type -> Type
recordApply v =
  roll
    $ TypeApp
        (constructor "Record")
        [ v ]

row :: Map RowLabel Type -> Maybe Type -> Row
row labels tail = Row { labels, tail }

string :: Type
string = roll TypeString

symbol :: String -> Type
symbol = roll <<< TypeSymbol

typeRow :: Row -> Type
typeRow = roll <<< TypeRow

typeRow' :: Map RowLabel Type -> Maybe Type -> Type
typeRow' fields = typeRow <<< row fields

var :: Ident -> Type
var = roll <<< TypeVar
