module Codegen.AST.Sugar where

import Prelude

import Codegen.AST.Types (Declaration(..), Expr, ExprF(..), Ident, Type, TypeF(..), TypeName)
import Data.Functor.Mu (roll)
import Data.Maybe (Maybe(..))

declType :: TypeName -> Array Ident -> Type -> { declaration :: Declaration , constructor :: Type }
declType typeName vars body =
  let
    declaration = DeclType
      { typeName, "type": body, vars }
    constructor = roll $ TypeConstructor { name: typeName, moduleName: Nothing }
  in
    { declaration, constructor }

declForeignData ∷ TypeName → { declaration ∷ Declaration, constructor ∷ Type }
declForeignData typeName =
  let
    declaration = DeclForeignData { typeName }
    constructor = roll $ TypeConstructor { name: typeName, moduleName: Nothing }
  in
    { declaration, constructor }

declValue ∷ Ident → Expr → Maybe Type → { declaration ∷ Declaration, var ∷ Expr }
declValue ident expr signature =
  let
    declaration = DeclValue
      { ident, expr, signature }
    var = roll $ ExprIdent { name: ident, moduleName: Nothing }
  in
    { declaration, var }

declForeignValue ∷ Ident → Type → { declaration ∷ Declaration, var ∷ Expr }
declForeignValue ident t =
  let
    declaration = DeclForeignValue { ident, "type": t }
    var = roll $ ExprIdent { name: ident, moduleName: Nothing }
  in
    { declaration, var }
