module Codegen.AST.Imports where

import Prelude
import Codegen.AST.Types (ClassName, Declaration(..), ExprF(..), Import(..), ImportDecl(..), Imports(..), ModuleName, QualifiedName, RowF(..), TypeF(..), TypeName)
import Data.Either (hush)
import Data.Foldable (fold, foldMap)
import Data.List (List)
import Data.List (fromFoldable) as List
import Data.Map (singleton, toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Set (singleton) as Set
import Data.Tuple (Tuple(..))
import Matryoshka (Algebra, cata)

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
  bei = foldMap (cata exprImportsAlgebra <<< _.value.expr) body

  bst = foldMap (maybe mempty (cata typeImportsAlgebra) <<< _.signature) body

  ti = foldMap (cata typeImportsAlgebra) types

  ci = fromMaybe mempty $ qualifiedClassNameImport className
declarationImports (DeclForeignData { typeName }) = mempty
declarationImports (DeclType { typeName, "type": t }) = cata typeImportsAlgebra t
declarationImports (DeclValue { value, signature: Just s }) = cata typeImportsAlgebra s <> cata exprImportsAlgebra value.expr
declarationImports (DeclValue { value, signature: Nothing }) = cata exprImportsAlgebra value.expr

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
  TypeNumber -> mempty
  TypeRow r -> rowImports r
  TypeRecord r -> rowImports r
  TypeString -> mempty
  TypeVar _ -> mempty
  where
  qualifiedTypeNameImport' = fromMaybe mempty <<< qualifiedTypeNameImport

  rowImports (Row r) =
    (fromMaybe mempty $ r.tail >>= hush >>= qualifiedTypeNameImport)
      <> (fold r.labels)

qualifiedTypeNameImport :: QualifiedName TypeName -> Maybe Imports
qualifiedTypeNameImport { moduleName: Just moduleName, name: t } = Just $ importsSingleton moduleName (ImportType t)
qualifiedTypeNameImport _ = Nothing

qualifiedClassNameImport :: QualifiedName ClassName -> Maybe Imports
qualifiedClassNameImport { moduleName: Just moduleName, name: c } = Just $ importsSingleton moduleName (ImportClass c)
qualifiedClassNameImport _ = Nothing

exprImportsAlgebra :: ExprF Imports -> Imports
exprImportsAlgebra = case _ of
  ExprBoolean _ -> mempty
  ExprApp f arg -> f <> arg
  ExprArray arr -> fold arr
  ExprIdent x -> fromMaybe mempty $ qualifiedValueImport x
  ExprNumber n -> mempty
  ExprRecord props -> fold props
  ExprString s -> mempty
  where
  qualifiedValueImport { moduleName: Just moduleName, name: t } = Just $ importsSingleton moduleName (ImportValue t)
  qualifiedValueImport _ = Nothing
