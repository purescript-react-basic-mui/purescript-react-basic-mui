module Codegen.AST.Sugar.Type where

import Prelude

import Codegen.AST.Types (Ident(..), ModuleName(..), QualifiedName, Row, RowF(..), RowLabel, Type, TypeF(..), TypeName)
import Data.Array (fromFoldable) as Array
import Data.Array (unsnoc)
import Data.Either (Either)
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

app ∷ Type → Array Type → Type
app n = roll <<< TypeApp n

arr ∷ Type → Type → Type
arr f = roll <<< TypeArr f

array ∷ Type → Type
array = roll <<< TypeArray

boolean ∷ Type
boolean = roll TypeBoolean

constructor ∷ String → Type
constructor = roll <<< TypeConstructor <<< name

constrained ∷ String → Array Type → Type → Type
constrained s params =
  roll <<< TypeConstrained { className: name' s, params }

forAll ∷ ∀ idents il names nl vars
  . HFoldl (List Ident → Ident → List Ident) (List Ident) (Record idents) (List Ident)
  ⇒ RowToList names nl
  ⇒ RowToList idents il
  ⇒ MapRecord nl names String Ident () idents
  ⇒ MapRecord il idents Ident Type () vars
  ⇒ Record names
  → (Record vars → Type)
  → Type
forAll names cont =
  let
    varsRecord = mapRecord Ident names
    toList = hfoldl (flip List.Cons ∷ List Ident → Ident → List Ident) (List.Nil ∷ List Ident)
    varsRecord' = mapRecord (roll <<< TypeVar) varsRecord
    idents = Array.fromFoldable (toList varsRecord)
  in
    roll (TypeForall idents (cont varsRecord'))

forAll' ∷ String → (Type → Type) → Type
forAll' n cont =
  let
    ident = Ident n
    v = roll $ TypeVar $ ident
  in
    roll (TypeForall [ ident ] (cont v))

name ∷ String → QualifiedName TypeName
name = name'

name' ∷ ∀ n. Newtype n String ⇒ String → QualifiedName n
name' n = qn n
  where
    qn = split (Pattern ".") >>> unsnoc >>> case _ of
      Just { init, last } →
        { name: wrap last
        , moduleName: case init of
            [] → Nothing
            otherwise → Just $ ModuleName $ intercalate "." init
        }
      Nothing → { name: wrap n, moduleName: Nothing }

number ∷ Type
number = roll TypeNumber

record ∷ Row → Type
record = roll <<< TypeRecord

recordApply ∷ Type → Type
recordApply v = roll $ TypeApp
  (constructor "Record")
  [ v ]

row ∷ Map RowLabel Type → Maybe (Either Ident (QualifiedName TypeName)) → Row
row labels tail = Row $ { labels: labels, tail }

rowType ∷ Row → Type
rowType = roll <<< TypeRow

string ∷ Type
string = roll TypeString

var ∷ Ident → Type
var = roll <<< TypeVar

