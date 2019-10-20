module Codegen.AST.Sugar where

import Prelude

import Codegen.AST.Printers (PrintingContext(..), printQualifiedName, printType)
import Codegen.AST.Types (ClassName, Declaration(..), Expr, ExprF(..), Ident(..), QualifiedName, Type, TypeF(..), TypeName, ValueBindingFields)
import Data.Foldable (foldMap)
import Data.Functor.Mu (roll)
import Data.Maybe (Maybe(..))
import Data.String.Extra (camelCase)
import Matryoshka.Fold (cata)

declType :: TypeName -> Array Ident -> Type -> { declaration :: Declaration , constructor :: Type }
declType typeName vars body =
  let
    declaration = DeclType
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

valueBindingFields :: Ident -> Array Ident -> Expr -> Maybe Type -> ValueBindingFields
valueBindingFields name binders expr signature = { value: { binders, expr, name }, signature }

declValue :: Ident -> Array Ident -> Expr -> Maybe Type -> { declaration :: Declaration, var :: Expr }
declValue name binders expr signature =
  let
    declaration = DeclValue (valueBindingFields name binders expr signature)
    var = roll $ ExprIdent { name, moduleName: Nothing }
  in
    { declaration, var }

declForeignValue :: Ident -> Type -> { declaration :: Declaration, var :: Expr }
declForeignValue ident t =
  let
    declaration = DeclForeignValue { ident, "type": t }
    var = roll $ ExprIdent { name: ident, moduleName: Nothing }
  in
    { declaration, var }

declInstance :: QualifiedName ClassName -> Array Type -> Array ValueBindingFields -> Declaration
declInstance className types body = DeclInstance
  { head:
    { className
    , name: Ident $ camelCase $
        printQualifiedName className <> foldMap (flip (cata printType) StandAlone) types
    , types
    }
  , body
  }

