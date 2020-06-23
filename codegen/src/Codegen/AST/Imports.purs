module Codegen.AST.Imports where

import Prelude

import Codegen.AST.Types (ClassName, Declaration(..), ExprF(..), Import(..), ImportDecl(..), Imports(..), ModuleName, QualifiedName, RowF(..), TypeF(..), TypeName, ValueBindingFields(..))
import Data.Foldable (fold, foldMap)
import Data.List (List)
import Data.List (fromFoldable) as List
import Data.Map (Map)
import Data.Map (singleton, toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Newtype (un)
import Data.Set (singleton) as Set
import Data.Tuple (Tuple(..))
import Matryoshka (Algebra, cata)

type Alias = String

type ImportAlias = Map ModuleName (Maybe Alias)

importsDeclarations :: Imports -> List ImportDecl
importsDeclarations (Imports i) = map step <<< Map.toUnfoldable $ i
  where
  step (Tuple moduleName names) = ImportDecl { moduleName, names: List.fromFoldable names }

importsSingleton :: ModuleName -> Import -> Imports
importsSingleton moduleName import_ = Imports (Map.singleton moduleName (Set.singleton import_))

declarationImports :: Declaration -> Imports
declarationImports (DeclForeignValue { ident, "type": t }) = cata typeImportsAlgebra t
declarationImports (DeclInstance { head: { className, types }, body }) = bei <> bst <> ti <> ci
  where
  bei = foldMap (cata exprImportsAlgebra <<< _.value.expr <<< un ValueBindingFields) body

  bst = foldMap (maybe mempty (cata typeImportsAlgebra) <<< _.signature <<< un ValueBindingFields) body

  ti = foldMap (cata typeImportsAlgebra) types

  ci = fromMaybe mempty $ qualifiedClassNameImport className
declarationImports (DeclForeignData { typeName }) = mempty
declarationImports (DeclType { typeName, "type": t }) = cata typeImportsAlgebra t
declarationImports (DeclValue vb) = valueBindingFieldsImports vb

valueBindingFieldsImports :: ValueBindingFields -> Imports
valueBindingFieldsImports (ValueBindingFields { value, signature }) =
  typeImports <> cata exprImportsAlgebra value.expr <> whereBindingsImports
  where
    whereBindingsImports = foldMap valueBindingFieldsImports value.whereBindings
    typeImports = case signature of
      Just s -> cata typeImportsAlgebra s
      Nothing ->mempty

typeImportsAlgebra :: Algebra TypeF Imports
typeImportsAlgebra = case _ of
  TypeApp l args -> l <> fold args
  TypeArray t -> t
  TypeArr f a -> f <> a
  TypeBoolean -> mempty
  TypeConstrained { className, params } t -> ci <> fold params <> t
    where
    ci = fromMaybe mempty $ qualifiedClassNameImport className
  TypeConstructor qn -> qualifiedTypeNameImport' qn
  TypeForall _ t -> t
  TypeKinded t _ -> t
  TypeNumber -> mempty
  TypeOpt t -> t
  TypeRow r -> rowImports r
  TypeRecord r -> rowImports r
  TypeString -> mempty
  (TypeSymbol _) -> mempty
  TypeVar _ -> mempty
  where
  qualifiedTypeNameImport' = fromMaybe mempty <<< qualifiedTypeNameImport

  rowImports (Row r) =
    (fromMaybe mempty r.tail <> fold r.labels)

qualifiedTypeNameImport :: QualifiedName TypeName -> Maybe Imports
qualifiedTypeNameImport { moduleName: Just moduleName, name: t } =
  Just $ importsSingleton moduleName (ImportType { typeName: t, importConstructors: false })
qualifiedTypeNameImport _ = Nothing

qualifiedClassNameImport :: QualifiedName ClassName -> Maybe Imports
qualifiedClassNameImport { moduleName: Just moduleName, name: c } = Just $ importsSingleton moduleName (ImportClass c)
qualifiedClassNameImport _ = Nothing

exprImportsAlgebra :: ExprF Imports -> Imports
exprImportsAlgebra = case _ of
  ExprBoolean _ -> mempty
  ExprApp f arg -> f <> arg
  ExprArray arr -> fold arr
  ExprIdent ident ->
    fromMaybe mempty $ qualifiedValueImport ident
  ExprNumber n -> mempty
  ExprRecord props -> fold props
  ExprString s -> mempty
  where
  -- | Don't import type constructors here - check if t starts with capital letter
  qualifiedValueImport { typeName: Just typeName, qualifiedName: { moduleName: Just moduleName }} =
    Just $ importsSingleton moduleName (ImportType { typeName, importConstructors: true })
  qualifiedValueImport { qualifiedName: { moduleName: Just moduleName, name: t }} =
    Just $ importsSingleton moduleName (ImportValue t)
  qualifiedValueImport _ = Nothing
