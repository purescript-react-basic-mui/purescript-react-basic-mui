-- | The current printing goal:
-- | * Do whatever is needed to force `purty` make the formatting.
-- |
-- | Not the current goals:
-- | * Be efficient.
-- | * Clear code structure.
-- |
module Codegen.AST.Printers where

import Prelude

import Codegen.AST.Imports (ImportAlias, declarationImports, importsDeclarations)
import Codegen.AST.Types (Associativity(..), Declaration(..), ExprF(..), Ident(..), Import(..), ImportDecl(..), Kind(..), Module(..), ModuleName(..), Precedence(..), QualifiedName, RowF(..), TypeF(..), TypeName(..), TypeVarBinding(..), ValueBindingFields(..), reservedNames)
import Control.Monad.Reader (ask)
import Control.Monad.Reader.Class (class MonadReader)
import Data.Array (concat)
import Data.Array (cons, fromFoldable, null, singleton, uncons, unsnoc) as Array
import Data.Char.Unicode (isUpper)
import Data.Either (Either(..), fromRight)
import Data.Filterable (partitionMap)
import Data.Foldable (foldMap, intercalate, length)
import Data.FoldableWithIndex (foldMapWithIndex)
import Data.List (intercalate, singleton) as List
import Data.Map (fromFoldableWith, lookup, toUnfoldable) as Map
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Monoid (guard)
import Data.Newtype (class Newtype, unwrap)
import Data.Set (member) as Set
import Data.String (joinWith)
import Data.String.CodeUnits (uncons) as Data.String.CodeUnits
import Data.String.Regex (Regex, regex)
import Data.String.Regex (test) as Regex
import Data.String.Regex.Flags (noFlags) as Regex.Flags
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Matryoshka (AlgebraM, cataM)
import Partial.Unsafe (unsafePartial)

lines :: Array String -> String
lines = joinWith "\n"

line :: Array String -> String
line = joinWith " "

printModule :: forall m. MonadReader ImportAlias m => Module -> m String
printModule (Module { moduleName, declarations }) = do
  declarations' <- traverse printDeclaration declarations
  imports' <- traverse printImport imports
  pure $ lines
    $ [ printModuleHead moduleName
      , ""
      , List.intercalate "\n" $ imports'
      , ""
      , List.intercalate "\n\n" $ declarations'
      ]
  where
  imports = importsDeclarations <<< foldMap (declarationImports) $ declarations

printImport :: forall m. MonadReader ImportAlias m => ImportDecl -> m String
printImport (ImportDecl { moduleName: ModuleName "Prelude" }) = pure "import Prelude"
printImport (ImportDecl { moduleName, names }) = do
  let
    mn = printModuleName moduleName
  alias <- ask >>= Map.lookup moduleName >>> case _ of
    Just Nothing -> pure Nothing
    Just a -> pure a
    Nothing -> pure (Just mn)
  pure $ line $
    [ "import"
    , mn
    , "(" <> (joinWith ", " <<< Array.fromFoldable $ (foldTypeImports typeImports <> otherImports)) <> ")"
    ]
    <> case alias of
      Just a -> [ "as", a ]
      Nothing -> []
  where
    partitionStep (ImportType { typeName, importConstructors }) = Left (Tuple typeName importConstructors)
    partitionStep (ImportClass s) = Right ("class " <> unwrap s)
    partitionStep (ImportValue s) = Right (unwrap s)

    printTypeImport (TypeName tn) importConstructors =
      (List.singleton (tn <> guard importConstructors "(..)"))

    foldTypeImports = foldMapWithIndex printTypeImport <<< Map.fromFoldableWith (||)

    { left: typeImports, right: otherImports } = partitionMap partitionStep $ names

indent :: String -> String
indent = append "  "

printModuleHead :: ModuleName -> String
printModuleHead moduleName = line [ "module", printModuleName moduleName, "where" ]

printDeclaration :: forall m. MonadReader ImportAlias m => Declaration -> m String
-- , "kind": k }) =
printDeclaration (DeclForeignData { typeName: TypeName name }) = pure $
  line [ "foreign import data", name, "::", "Type" ] -- fromMaybe "Type" k
-- , "kind": k }) =
printDeclaration (DeclForeignValue { ident: Ident ident, "type": t }) = do
  p <- cataM printType t <@> StandAlone
  pure $ line $ [ "foreign import", ident, "::" ] <> p -- fromMaybe "Type" k
printDeclaration (DeclInstance { head, body }) = do
  cp <- printQualifiedName head.className
  params <- for head.types \t ->
    cataM printType t <@> InApplication
  let
    h =
      line
        [ "instance"
        , unwrap head.name
        , "::"
        , cp
        , line (map line params)
        , "where"
        ]
  body' <- for body \d ->
    map indent <$> printValueBindingFields d
  pure $ lines $ Array.cons h (concat body')
printDeclaration (DeclValue v) = lines <$> printValueBindingFields v
printDeclaration (DeclType { typeName, "type": t, vars }) = do
  body <- cataM printType t <@> StandAlone
  pure $ lines $ [ line [ "type", unwrap typeName, line $ map printTypeVarBinding vars, "=" ] ] <> map indent body
  where
    printTypeVarBinding (TypeVarName (Ident n)) = n
    printTypeVarBinding (TypeVarKinded { label: Ident l, "kind": k }) = "(" <> l  <> " :: " <> printKind k <> ")"

printKind :: Kind -> String
printKind KindRow = "# Type"
printKind (KindName n) = n

printValueBindingFields :: forall m. MonadReader ImportAlias m => ValueBindingFields -> m (Array String)
printValueBindingFields (ValueBindingFields { value: { binders, name: Ident name, expr, whereBindings }, signature }) = do
  e <- cataM printExpr expr <@> { precedence: Zero, binary: Nothing }
  w <- whereBindingsSection
  let
    vb = name <> bs <> " = " <> e
  s <- case signature of
    Nothing -> pure mempty
    Just s -> do
      p <- cataM printType s <@> StandAlone
      pure $ case Array.uncons p of
        Just { head, tail } -> [ name <> "::" <> head ] <> tail
        otherwise -> [ name <> "::"] <> p
  pure $ s <> [ vb ] <> w
  where
    bs = if Array.null binders then mempty else " " <> line (map unwrap binders)
    whereBindingsSection = do
      w <- for whereBindings \b ->
        map (indent <<< indent) <$> printValueBindingFields b
      pure $ case w of
        [] -> []
        otherwise -> Array.cons (indent "where") (concat w)


printModuleName :: ModuleName -> String
printModuleName (ModuleName n) = n

printQualifiedName :: forall m n. Newtype n String => MonadReader ImportAlias m => QualifiedName n -> m String
printQualifiedName { moduleName: Nothing, name } = pure $ unwrap name
printQualifiedName { moduleName: Just m, name } = case m of
  (ModuleName "Prelude") -> pure $ unwrap name
  mn -> do
    aliases <- ask
    pure $ case Map.lookup mn aliases of
      Just Nothing -> unwrap name
      Just (Just alias) -> alias <> "." <> unwrap name
      otherwise -> printModuleName mn <> "." <> unwrap name

data Branch
  = BranchLeft
  | BranchRight

-- | I'm not sure about this whole minimal parens printing strategy
-- | so please correct me if I'm wrong.
type ExprPrintingContext
  = { precedence :: Precedence
    , binary ::
      Maybe
        { assoc :: Associativity
        , branch :: Branch
        }
    }

-- | We are using here GAlgebra to get
-- | a bit nicer output for an application but
-- | probably we should just use the same strategy
-- | as in case of `printType` and pass printing context.
printExpr :: forall m. MonadReader ImportAlias m => AlgebraM m ExprF (ExprPrintingContext -> String)
printExpr = case _ of
  ExprBoolean b -> pure $ const (show b)
  ExprApp x y -> pure $ case _ of
    { precedence, binary: Just { assoc, branch } } -> case precedence `compare` Six, assoc, branch of
      GT, _, _ -> "(" <> sub <> ")"
      EQ, AssocLeft, BranchRight -> "(" <> sub <> ")"
      EQ, AssocRight, BranchLeft -> "(" <> sub <> ")"
      _, _, _ -> sub
    { precedence } ->
      if precedence < Six then
        sub
      else
        "(" <> sub <> ")"
    where
    sub =
      x { precedence: Six, binary: Just { assoc: AssocLeft, branch: BranchLeft } }
        <> " "
        <> y { precedence: Six, binary: Just { assoc: AssocLeft, branch: BranchRight } }
  ExprArray arr -> pure $ const $ "[" <> intercalate ", " (map (_ $ zero) arr) <> "]"
  ExprIdent x -> const <$> printQualifiedName x.qualifiedName
  ExprNumber n -> pure $ const (show n)
  ExprRecord props -> pure $ const $ "{ " <> intercalate ", " props' <> " }"
    where
    props' :: Array String
    props' = map (\(Tuple n v) -> printRowLabel n <> ": " <> v zero) <<< Map.toUnfoldable $ props
  ExprString s -> pure $ const $ show s
  where
  zero = { precedence: Zero, binary: Nothing }

data PrintingContext
  = StandAlone
  | InApplication
  | InArr

printRowLabel :: String -> String
printRowLabel l = case Data.String.CodeUnits.uncons l of
  Just { head } ->
    if isUpper head || l `Set.member` reservedNames || not (Regex.test alphanumRegex l) then
      show l
    else
      l
  Nothing -> l
  where
  alphanumRegex :: Regex
  alphanumRegex = unsafePartial $ fromRight $ regex "^[A-Za-z0-9_]*$" Regex.Flags.noFlags

printType :: forall m. MonadReader ImportAlias m => AlgebraM m TypeF (PrintingContext -> Array String)
printType = case _ of
  TypeApp l params -> pure $ Array.singleton <<< parens (line $ (l InApplication) <> foldMap (_ $ InApplication) params)
  TypeArray t -> pure $ Array.singleton <<<  parens (line (Array.cons "Array " $ t StandAlone))
  TypeArr f a -> pure $ case _ of
    InArr -> [ "(" <> line s <> ")" ]
    InApplication -> [ "(" <> line s <> ")" ]
    otherwise -> [ line s ]
    where
    s = f InArr <> [ " -> " ] <> a StandAlone
  TypeBoolean -> pure $ const [ "Boolean" ]
  TypeConstructor qn -> const <<< Array.singleton <$> printQualifiedName qn
  TypeConstrained { className, params } t -> do
    constraint <- printQualifiedName className
    let
      params' = foldMap (_ $ InApplication) params
    pure $ const $
      [ line $ [ constraint ] <> params' <> [ "=>" ] ]
      <>
      t StandAlone
  TypeForall vs t -> pure $ const $ case vs of
    [] -> t StandAlone
    otherwise ->
      [ "forall" <> " " <> line (map unwrap vs) <> "." <> " " ] <> map indent (t StandAlone)
  TypeKinded t k -> pure $ const $ [ line $ [ "(" ] <> t StandAlone <> [ " :: " <> printKind k <> ")" ]]
  TypeNumber -> pure $ const [ "Number" ]
  TypeOpt t -> pure $ case _ of
    InApplication -> [ "(" <> s <> ")" ]
    otherwise -> [ s ]
    where
      -- | We are dropping support for optionals because of:
      -- | https://discourse.purescript.org/t/rowlist-iteration-seems-to-be-relatively-slow/1492/4
      -- s = "Opt (" <> line (t StandAlone) <> ")"
      s = line (t StandAlone)
  TypeRecord r -> pure $ const $ [ "{ " ] <> printRow r <> [ " }" ]
  TypeRow r@(Row { labels, tail })  -> pure $ const $ case length labels, tail of
    0, Nothing -> [ "()" ]
    0, Just t -> t StandAlone
    _, _ -> [ "( " ] <> printRow r <> [ " )" ]
  TypeString -> pure $ const [ "String" ]
  (TypeSymbol s) -> pure $ const [ show s ]
  TypeVar (Ident v) -> pure $ const $ [ v ]

  where
    parens s InArr = s
    parens s InApplication = "(" <> s <> ")"
    parens s StandAlone = s

    printRow :: _ -> Array String
    printRow (Row { labels, tail }) = map indent (mapButLast (_ <> ",") labels') <> fromMaybe mempty ((\p -> append " | " <$> p StandAlone) <$> tail)
      where
      mapButLast :: forall a. (a -> a) -> Array a -> Array a
      mapButLast f arr = case Array.unsnoc arr of
        Just { init, last } -> map f init <> [ last ]
        Nothing -> []

      labels' :: Array String
      labels' = map (\(Tuple n t) -> printRowLabel n <> " :: " <> line (t StandAlone)) <<< Map.toUnfoldable $ labels

