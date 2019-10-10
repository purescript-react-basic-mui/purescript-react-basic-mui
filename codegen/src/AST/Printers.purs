module Codegen.AST.Printers where

import Prelude

import Codegen.AST.Imports (declarationImports, importsDeclarations)
import Codegen.AST.Types (Declaration(..), Expr, ExprF(..), Ident(..), Import(..), ImportDecl(..), Module(..), ModuleName(..), QualifiedName, RowF(..), TypeF(..), TypeName(..), reservedNames)
import Data.Array (cons, fromFoldable) as Array
import Data.Char.Unicode (isUpper)
import Data.Either (Either(..))
import Data.Foldable (foldMap, intercalate)
import Data.Functor.Mu (Mu(..)) as Mu
import Data.List (intercalate) as List
import Data.Map (toUnfoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap)
import Data.Set (member) as Set
import Data.String (joinWith)
import Data.String.CodeUnits (uncons) as SCU
import Data.Tuple (Tuple(..), snd)
import Matryoshka (Algebra, GAlgebra, cata, para)

lines ∷ Array String → String
lines = joinWith "\n"

line ∷ Array String → String
line = joinWith " "

printModule ∷ Module → String
printModule (Module { moduleName, declarations }) = lines $
  [ printModuleHead moduleName
  , ""
  , List.intercalate "\n" $ map printImport imports
  , ""
  , List.intercalate "\n\n" $ map printDeclaration declarations
  ]
  where
    imports = importsDeclarations <<< foldMap (declarationImports) $ declarations

printImport ∷ ImportDecl → String
printImport (ImportDecl { moduleName, names }) = line
  [ "import"
  , mn
  , "(" <> (joinWith ", " <<< Array.fromFoldable <<< map printName $ names) <> ")"
  , "as"
  , mn
  ]
  where
    mn = printModuleName moduleName

    printName ∷ Import → String
    printName (ImportValue s) = unwrap s
    printName (ImportClass s) = "class " <> unwrap s
    printName (ImportType s) = unwrap s

printModuleHead :: ModuleName -> String
printModuleHead moduleName =
  line ["module", printModuleName moduleName, "where" ]

printDeclaration ∷ Declaration → String
printDeclaration (DeclForeignData { typeName: TypeName name }) = -- , "kind": k }) =
  line [ "foreign import data", name, "::", "Type" ] -- fromMaybe "Type" k
printDeclaration (DeclForeignValue { ident: Ident ident, "type": t }) = -- , "kind": k }) =
  line [ "foreign import", ident, "::", cata printType t StandAlone ] -- fromMaybe "Type" k
printDeclaration (DeclValue { ident: Ident name, expr, signature }) =
  case signature of
    Nothing → v
    Just s →
      name <> " :: " <> cata printType s StandAlone
      <> "\n"
      <> v
  where
    v = name <> " = " <> para printExpr expr
printDeclaration (DeclType { typeName, "type": t, vars }) =
  line ["type", unwrap typeName, line $ (map unwrap) vars, "=", cata printType t StandAlone]
printDeclaration _ = "UNSUPPORTED YET"

printModuleName ∷ ModuleName → String
printModuleName (ModuleName n) = n

printQualifiedName ∷ ∀ n. Newtype n String ⇒ QualifiedName n → String
printQualifiedName { moduleName: Nothing, name } = unwrap name
printQualifiedName { moduleName: Just m, name } =
  printModuleName m <> "." <> unwrap name

-- | We are using here GAlgebra to get
-- | a bit nicer output for an application but
-- | probably we should just use the same strategy
-- | as in case of `printType` and pass printing context.
printExpr ∷ GAlgebra (Tuple Expr) ExprF String
printExpr = case _ of
  ExprBoolean b → show $ b
  ExprApp (Tuple (Mu.In (ExprApp _ _)) x) y → "(" <> x <> ") (" <> snd y <> ")"
  ExprApp x (Tuple (Mu.In (ExprApp _ _)) y) → snd x <> ") (" <> y <> ")"
  ExprApp x y → snd x <> " " <> snd y
  ExprArray arr → "[" <> intercalate ", " (map snd arr) <> "]"
  ExprIdent x → printQualifiedName x
  ExprNumber n →  show n
  ExprRecord props → "{ " <> intercalate ", " props' <> " }"
    where
      props' ∷ Array String
      props' = map (\(Tuple n v) → n <> ": " <> snd v) <<< Map.toUnfoldable $ props
  ExprString s → show s

data PrintingContext = StandAlone | InApplication

printType ∷ Algebra TypeF (PrintingContext → String)
printType = case _ of
  TypeApp l params -> parens (line $ Array.cons (l InApplication) (map (_ $ InApplication) params))
  TypeArray t -> parens $ "Array "  <> t StandAlone
  TypeArr f a -> parens $ f StandAlone <> " → " <> a StandAlone
  TypeBoolean -> const "Boolean"
  TypeConstructor qn -> const $ printQualifiedName qn
  TypeConstrained { className, params } t -> const $
    printQualifiedName className <> " " <> line (map (_ $ InApplication) params) <> " ⇒ " <> t StandAlone
  TypeForall vs t → const $ "∀" <> " " <> line (map unwrap vs) <> "." <> " " <> t StandAlone
  TypeNumber -> const "Number"
  TypeRecord r -> const $ "{ " <> printRow r <> " }"
  TypeRow r -> const $ "( " <> printRow r <> " )"
  TypeString -> const $ "String"
  TypeVar (Ident v) -> const $ v
  where
    parens s InApplication = "(" <> s <> ")"
    parens s StandAlone = s

    printRow (Row { labels, tail }) = intercalate ", " labels' <> tail'
      where
        labels' ∷ Array String
        labels' = map (\(Tuple n t) → label n <> " ∷ " <> t StandAlone) <<< Map.toUnfoldable $ labels

        label l = case SCU.uncons l of
          Just { head } → if isUpper head || l `Set.member` reservedNames
            then show l
            else l
          Nothing → l

        tail' = case tail of
          Nothing → mempty
          Just (Left ident) → " | " <> unwrap ident
          Just (Right qn) → " | " <> printQualifiedName qn
