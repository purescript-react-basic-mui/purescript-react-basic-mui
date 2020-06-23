module Codegen.AST.Sugar where

import Prelude

import Codegen.AST.Imports (ImportAlias)
import Codegen.AST.Printers (PrintingContext(..), line, printQualifiedName, printType)
import Codegen.AST.Types (ClassName, Declaration(..), Expr, ExprF(..), Ident(..), ModuleName(..), QualifiedName, Type, TypeF(..), TypeName(..), TypeVarBinding, ValueBindingFields(..))
import Control.Monad.Reader (class MonadReader)
import Data.Array (fromFoldable, unsnoc) as Array
import Data.Foldable (foldMap)
import Data.Functor.Mu (roll)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..), joinWith, split) as String
import Data.String.Extra (camelCase)
import Data.Traversable (for)
import Matryoshka (cataM)
import Prim.Row (class Cons, class Lacks) as Row
import Record.Builder (Builder) as Record
import Record.Builder (build, insert) as Record.Builder
import Record.Extra (type (:::), SNil, kind SList)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)

declType :: TypeName -> Array TypeVarBinding -> Type -> { declaration :: Declaration, constructor :: Type }
declType typeName vars body =
  let
    declaration =
      DeclType
        { typeName, "type": body, vars }

    constructor = roll $ TypeConstructor { name: typeName, moduleName: Nothing }
  in
    { declaration, constructor }

declForeignData :: TypeName -> { declaration :: Declaration, constructor :: Type }
declForeignData typeName =
  let
    declaration = DeclForeignData { typeName }

    constructor = roll $ TypeConstructor { name: typeName, moduleName: Nothing }
  in
    { declaration, constructor }

declForeignData' :: String -> { declaration :: Declaration, constructor :: Type }
declForeignData' = declForeignData <<< TypeName

valueBindingFields :: Ident -> Array Ident -> Expr -> Maybe Type -> Array ValueBindingFields -> ValueBindingFields
valueBindingFields name binders expr signature whereBindings = ValueBindingFields
  { value: { binders, expr, name, whereBindings }, signature }

declValue :: Ident -> Array Ident -> Expr -> Maybe Type -> Array ValueBindingFields -> { declaration :: Declaration, var :: Expr }
declValue name binders expr signature whereBindings =
  let
    declaration = DeclValue (valueBindingFields name binders expr signature whereBindings)

    var = roll $ ExprIdent { typeName: Nothing, qualifiedName: { name, moduleName: Nothing }}
  in
    { declaration, var }

declForeignValue :: Ident -> Type -> { declaration :: Declaration, var :: Expr }
declForeignValue i t =
  let
    declaration = DeclForeignValue { ident: i, "type": t }

    var = roll $ ExprIdent
      { typeName: Nothing, qualifiedName: { name: i, moduleName: Nothing }}
  in
    { declaration, var }

declInstance :: forall m. MonadReader ImportAlias m => QualifiedName ClassName -> Array Type -> Array ValueBindingFields -> m Declaration
declInstance className types body = do
  types' <- for types \t ->
    cataM printType t <@> StandAlone
  cn <- printQualifiedName className
  pure $ DeclInstance
    { head:
      { className
      , name:
        Ident $ camelCase $ cn <> foldMap line types'
      , types
      }
    , body
    }

ident :: String -> Ident
ident = Ident

qualifiedIdent :: String -> QualifiedName Ident
qualifiedIdent s = case Array.unsnoc (String.split (String.Pattern ".") s) of
  Just { init: [], last } -> { moduleName: Nothing, name: Ident last }
  Nothing -> { moduleName: Nothing, name: Ident s }
  Just { init, last } -> { moduleName: Just $ ModuleName (String.joinWith "." init), name: Ident last }

local :: forall a. a -> QualifiedName a
local name = { moduleName: Nothing, name }

data SListProxy (l :: SList) = SListProxy

class ForallRow (l :: SList) (r :: # Type) (r' :: # Type) | l -> r r' where
  forallRow :: SListProxy l -> { binders :: List Ident, builder :: Record.Builder { | r } { | r' }}

instance forallRowNil :: ForallRow SNil () () where
  forallRow _ = { binders: Nil, builder: identity }

instance forallRowCons :: (ForallRow t tr_ tr, IsSymbol h, Row.Lacks h tr, Row.Cons h Type tr r) => ForallRow (h ::: t) tr_ r where
  forallRow _ =
    let
      _h = (SProxy :: SProxy h)
      h = reflectSymbol _h
      { binders, builder } = forallRow (SListProxy :: SListProxy t)
    in
      { binders: Ident h : binders
      , builder: Record.Builder.insert _h
          (roll $ TypeVar $ ident h) <<< builder
      }

class BindersRow (l :: SList) (r :: # Type) (r' :: # Type) | l -> r r' where
  bindersRow :: SListProxy l -> { binders :: List Ident, builder :: Record.Builder { | r } { | r' }}

instance bindersRowNil :: BindersRow SNil () () where
  bindersRow _ = { binders: Nil, builder: identity }

instance bindersRowCons :: (BindersRow t tr_ tr, IsSymbol h, Row.Lacks h tr, Row.Cons h Expr tr r) => BindersRow (h ::: t) tr_ r where
  bindersRow _ =
    let
      _h = (SProxy :: SProxy h)
      h = reflectSymbol _h
      { binders, builder } = bindersRow (SListProxy :: SListProxy t)
    in
      { binders: Ident h : binders
      , builder: Record.Builder.insert _h
          (roll $ ExprIdent $ { typeName: Nothing, qualifiedName: qualifiedIdent h}) <<< builder
      }


-- | Turns given record of strings into a record of type variables
-- | which are passed to type building function.
-- | Finally wrap its result with `ForAll`.
-- |
-- | ```
-- | signature = forAll { g: "given", r: "required"} \{ g, r } ->
-- |  let
-- |    fun = arr (recordApply g) (constructor "ResultType")
-- |  in
-- |    constrained "Prim.Row.Union" [ g, r, constructor "FinalRow" ] fun
-- |
-- | ```
-- | Gives us:
-- |
-- | forall required given. Prim.Row.Union given required FinalRow => Record given -> ResultType
-- |
forAllValueBinding ::
  forall bindersList varsRow typeBindersList typeVarsRow.
  BindersRow bindersList () varsRow =>
  ForallRow typeBindersList () typeVarsRow =>
  SListProxy typeBindersList ->
  String ->
  SListProxy bindersList ->
  ({ typeVars :: { | typeVarsRow }, vars :: { | varsRow } }
    -> { signature :: Maybe Type, expr :: Expr, whereBindings :: Array ValueBindingFields }) ->
  ValueBindingFields
forAllValueBinding typeBinders name binders cont =
  let
    typeVars =
      let
        tb = forallRow typeBinders
      in
        { binders: Array.fromFoldable tb.binders
        , bindersVars: Record.Builder.build tb.builder {}
        }
    vars =
      let
        bb = bindersRow binders
      in
        { binders: Array.fromFoldable bb.binders
        , bindersVars: Record.Builder.build bb.builder {}
        }
    vb = cont { typeVars: typeVars.bindersVars, vars: vars.bindersVars }
  in ValueBindingFields
    { signature: (roll <<< TypeForall typeVars.binders <$> vb.signature)
    , value:
      { name: Ident name
      , binders: vars.binders
      , expr: vb.expr
      , whereBindings: vb.whereBindings
      }
    }
