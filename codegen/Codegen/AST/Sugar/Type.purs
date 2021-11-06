module Codegen.AST.Sugar.Type where

import Prelude

import Codegen.AST.Types (Fields, Ident(..), ModuleName(..), QualifiedName, Row, RowF(..), RowLabel, Typ, TypeF(..), TypeName)
import Data.Array (fromFoldable) as Array
import Data.Array (unsnoc)
import Data.Foldable (intercalate)
import Data.Functor.Mu (roll)
import Data.List (List(..)) as List
import Data.List (List)
import Data.Map (Map)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap)
import Data.String (Pattern(..), split)
import Heterogeneous.Folding (class HFoldl, hfoldl)
import Prim.RowList (class RowToList)
import Record.Extra (class MapRecord, mapRecord)

app :: Typ -> Array Typ -> Typ
app n = roll <<< TypeApp n

arr :: Typ -> Typ -> Typ
arr f = roll <<< TypeArr f

array :: Typ -> Typ
array = roll <<< TypeArray

boolean :: Typ
boolean = roll TypeBoolean

constructor :: String -> Typ
constructor = roll <<< TypeConstructor <<< name

constrained :: String -> Array Typ -> Typ -> Typ
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
-- -- | forall required given. Prim.Row.Union given required FinalRow => Record given -> ResultTyp
-- -- |
forAll ::
  forall idents il names nl vars.
  HFoldl (List Ident -> Ident -> List Ident) (List Ident) (Record idents) (List Ident) =>
  RowToList names nl =>
  RowToList idents il =>
  MapRecord nl names String Ident () idents =>
  MapRecord il idents Ident Typ () vars =>
  Record names ->
  (Record vars -> Typ) ->
  Typ
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
  MapRecord il idents Ident Typ () vars =>
  Array Ident ->
  Record names ->
  (Record vars -> Typ) ->
  Typ
forAllWith extraIdents names cont =
  let
    varsRecord = mapRecord Ident names

    toList = hfoldl (flip List.Cons :: List Ident -> Ident -> List Ident) (List.Nil :: List Ident)

    varsRecord' = mapRecord (roll <<< TypeVar) varsRecord

    idents' = extraIdents <> Array.fromFoldable (toList varsRecord)
  in
    roll (TypeForall idents' (cont varsRecord'))

forAll' :: String -> (Typ -> Typ) -> Typ
forAll' n cont =
  let
    ident = Ident n

    v = roll $ TypeVar $ ident
  in
    roll (TypeForall [ ident ] (cont v))

int :: Typ
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

number :: Typ
number = roll TypeNumber

record :: Row -> Typ
record = roll <<< TypeRecord

recordLiteral :: Typ -> Typ
recordLiteral tail = record $ Row { labels: M.empty, tail: Just tail }

recordLiteral' :: Fields Typ -> Typ -> Typ
recordLiteral' fields tail = record $ Row { labels: fields, tail: Just tail }

recordApply :: Typ -> Typ
recordApply v =
  roll
    $ TypeApp
        (constructor "Record")
        [ v ]

row :: Map RowLabel Typ -> Maybe Typ -> Row
row labels tail = Row { labels, tail }

string :: Typ
string = roll TypeString

symbol :: String -> Typ
symbol = roll <<< TypeSymbol

typeRow :: Row -> Typ
typeRow = roll <<< TypeRow

typeRow' :: Map RowLabel Typ -> Maybe Typ -> Typ
typeRow' fields = typeRow <<< row fields

var :: Ident -> Typ
var = roll <<< TypeVar
