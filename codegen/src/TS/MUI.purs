module Codegen.TS.MUI where

import Prelude

import Codegen.AST (ClassName(..), Declaration(..), Ident(..), ModuleName(..), Row, Type, TypeF(..), TypeName(..))
import Codegen.AST (Declaration(..), Module(..), ModuleName(..), RowF(..), TypeName(..), Union(..)) as AST
import Codegen.AST.Sugar (declForeignData, declForeignValue, declType, declValue)
import Codegen.AST.Sugar.Expr (app, ident) as Expr
import Codegen.AST.Sugar.Type (app, arr, constructor, record, row, string) as Type
import Codegen.AST.Sugar.Type (arr, forAll, row) as T
import Codegen.AST.Sugar.Type (constrained, forAll, forAll', recordApply)
import Codegen.Model (Component, psImportPath, reactComponentApply)
import Codegen.TS.Module (astAlgebra, PossibleType(..), declarations, unionDeclarations) as TS.Module
import Codegen.TS.Module (declarations, exprUnsafeCoerce)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.State (runState)
import Data.Array (elem, fromFoldable, singleton, toUnfoldable) as Array
import Data.Either (Either(..))
import Data.Foldable (class Foldable, foldr, intercalate)
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Mu (roll)
import Data.List (List(..), singleton, snoc) as List
import Data.List (List)
import Data.Map (filterKeys, fromFoldable, lookup, singleton) as Map
import Data.Map.Internal (keys) as Map.Internal
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Moldy (Moldy(..))
import Data.Newtype (unwrap)
import Data.Predicate (Predicate(..))
import Data.String (joinWith)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceM)
import Effect (Effect)
import Heterogeneous.Folding (class FoldlRecord, class HFoldl, ConstFolding(..), hfoldl)
import Matryoshka (cata, cataM)
import Node.FS (FileFlags(..))
import Prim.RowList (class RowToList)
import ReadDTS.Instantiation (TypeF(..)) as Instantation
import ReadDTS.Instantiation.Pretty (pprintTypeName)
import Record.Extra (class MapRecord, mapRecord)

type ComponentName = String
type TsImportPath = String

tsImportPath ∷ ComponentName → TsImportPath
tsImportPath component =
  "@material-ui/core/" <> component <> "/" <> component

psModuleName ∷ ComponentName → ModuleName
psModuleName component = ModuleName $ "MUI.Core." <> component
--   { name ∷ String
--   , moduleName ∷ ModuleName
--   , propsType ∷
--     { baseRow ∷ Maybe Row
--     , generate ∷ Array RowLabel
--     , vars ∷ Array AST.Ident
--     }
--   , componentTypeVariables ∷ Array AST.Ident
--   -- , additionalTypeVariables ∷ Array String
--   -- , classKey ∷ Array String
--   -- , inherits ∷ PropType
--   -- , variants ∷ Array Variant
--   , extraCode ∷ Maybe (Array Declaration)

-- componentAST ∷ ComponentName → Maybe AST.Row → Set AST.RowLabel → ExceptT (Array String) Effect AST.Module
type M a = ExceptT (Array String) Effect a
componentAST ∷ Component → M AST.Module
componentAST { name: componentName, inherits, propsType: { base, generate, vars }} = do
  tsDeclarations ← TS.Module.declarations
    { path: instanceModulePath
    , source: Just source
    }
  case Map.lookup instanceTypeName tsDeclarations of
    Nothing → throwError $ Array.singleton $ line
      ["Unable to find generated props instance type:", show instanceTypeName ]
    Just { defaultInstance } → case defaultInstance of
        Mu.In (Instantation.Object n props) → do
          { prop: classesProp, declarations: classesDeclarations } ← if "classes" `Array.elem` generate
            then classesPropAST inherits props
            else pure { prop: Nothing, declarations: List.Nil }

          let
            -- | Take only a subset of props using given label set.
            props' = Map.filterKeys ((&&) <$> (not <<< eq "classes") <*> (_ `Array.elem` generate)) props
            -- | Create an new "Object" type from them
            -- | for AST generation.
            obj = roll $ Instantation.Object n props'
            types = flip runState mempty <<< runExceptT <<< cataM TS.Module.astAlgebra $ obj

          case types of
            Tuple (Right (TS.Module.ProperType (Mu.In (TypeRecord (AST.Row { labels, tail: Nothing }))))) unions → do
              let
                AST.Row base' = fromMaybe (T.row mempty Nothing) base
                classes = case classesProp of
                  Just p → Map.singleton "classes" p
                  Nothing -> mempty
                c' = Type.record $ Type.row (classes <> labels <> base'.labels) base'.tail
                propsTypeDecl = AST.DeclType
                  { typeName: AST.TypeName "PropsOptions", "type": c', vars }

              unions' ← for unions $ case _ of
                AST.Union { moduleName: Just _, name } _ → throwError $
                  [ "External union generation not implmented yet..." ]
                AST.Union { moduleName: Nothing, name } members →
                  pure $ TS.Module.unionDeclarations name members

              let
                step { "type": union, constructors } res =
                  List.Cons union (List.Cons constructors res)
                declarations = foldr step (List.singleton propsTypeDecl) unions' <> classesDeclarations

              pure $ AST.Module $
                { declarations
                , moduleName: psModuleName componentName
                }

            (Tuple (Right result) _) → throwError $ Array.singleton $ line $
              ["Expecting object type for props instance"
              , show instanceTypeName, "but got"
              , show result
              ]
            (Tuple (Left err) _) → throwError [ err ]

        Mu.In Instantation.Any → throwError $ Array.singleton $ lines
          [ line ["Props instance type is derived to be 'any' for" , show instanceTypeName]
          , line
            [ "Is it possible that your component path is broken"
            , "or exported props type has different name?"
            ]
          , "Generated ts source code was:"
          , source
         ]

        otherwise → throwError $ Array.singleton $ lines
          [ line
            ["Props instance type" , show instanceTypeName
            , "is not an object. Derived type is: ", show $ cata pprintTypeName defaultInstance
            ]
          , "Generated ts source code was:"
          , source
          ]
  where
    propsTypeName = componentName <> "Props"
    instanceTypeName = propsTypeName <> "Instance"
    instanceModulePath = instanceTypeName <> ".d.ts"
    -- | This approach is described in `Codegen.Typescript.Module`
    source = lines $
      [ line ["import", "{",  propsTypeName, "}", "from", show $ tsImportPath componentName ]
      , line ["export type", instanceTypeName, "=", propsTypeName <> ";"]
      ]

line = joinWith " "

lines = joinWith "\n"

-- | Generates all declarations related to classes.
-- |
-- | We are extracting classes directly from AST of a Props object.
-- | We could do the same on the generated instance but then we would
-- | be forced to remove `classes` from props
-- | before running `astAlgebra` on it. Classes record
-- | does not translate directly to any expected PS
-- | construct because it contains `any` types.
classesPropAST ∷ _ → _ → M _
classesPropAST inherits props = case Map.lookup "classes" props of
  Just { "type": Mu.In (Instantation.Object _ classesProps) } → do
    let
      classesNames = Map.Internal.keys classesProps
      binder = Ident "a"
      var = roll $ TypeVar binder
      -- Construct row type which looks like this:
      -- `type ClassKeyOptions a = ( root :: a, colorPrimary :: a)`

      classesGenericOptionsRow
        = roll
        <<< TypeRow
        <<< flip Type.row Nothing
        <<< Map.fromFoldable
        <<< map (flip Tuple var)
        $ classesNames

      classKeyGenericOptionsType = declType
        (TypeName "ClassKeyGenericOptions")
        [ binder ]
        classesGenericOptionsRow

      classKeyJSSOptionsType = declType
        (TypeName "ClassKeyOptionsJSS") [] (Type.app classKeyGenericOptionsType.constructor [ Type.constructor "MUI.Core.JSS" ])

      classKeyOptionsType = declType
        (TypeName "ClassKeyOptions") [] (Type.app classKeyGenericOptionsType.constructor [ Type.string ])

      -- | Construct a type and related constructor function which which looks like this:
      -- | ```
      -- | foreign import data BadgeClassKey
      -- |
      -- | classKey :: ∀  given required
      -- |  .  Union given required (BadgeClassKeyOptions )
      -- |  => Record given
      -- |  -> BadgeClassKey
      -- | classKey = unsafeCoerce
      -- | ```
      classKeyType = declForeignData (TypeName "ClassKey")
      classKeyValue =
        let
          ident = Ident "classKey"
          signature = forAll { g: "given", r: "required"} $ \{ g, r } →
            let
              fun = T.arr (recordApply g) classKeyType.constructor
            in
              constrained "Prim.Row.Union" [ g, r, classKeyOptionsType.constructor ] fun
        in
          declValue ident exprUnsafeCoerce (Just signature)

      classKeyJSSType = declForeignData (TypeName "ClassKeyJSS")
      classKeyJSSValue =
        let
          ident = Ident "classKeyJSS"
          signature = forAll { g: "given", r: "required"} $ \{ g, r } →
            let
              fun = T.arr (recordApply g) classKeyJSSType.constructor
            in
              constrained "Prim.Row.Union" [ g, r, classKeyJSSOptionsType.constructor ] fun
        in
          declValue ident exprUnsafeCoerce (Just signature)

      componentValue = declForeignValue (Ident "_Component") (forAll' "a" \a → reactComponentApply [a])

      -- | appBar :: ∀  given required
      -- |   .  Union given required (AppBarPropsOptions (PaperProps Props_div) )
      -- |   => Record given
      -- |   -> JSX
      -- | appBar = element _AppBar
      componentConstructor =
        let
          signature = forAll { g: "given", r: "required"} $ \{ g, r } →
            let
              fun = Type.arr (recordApply g) classKeyJSSType.constructor
              inherits' = case inherits of
                Nothing → Type.constructor "React.Basic.Props_div"
                Just t → t
              u = Type.app (Type.constructor "PropsOptions") [ inherits' ]
            in
              constrained "Prim.Row.Union" [ g, r, u] fun
        in
          declValue
            (Ident "component")
            (Expr.app (Expr.ident "React.Basic.element") componentValue.var)
            (Just signature)

      componentConstructor' =
        let
          signature = forAll { c: "componentProps", g: "given", r: "required"} $ \{ c, g, r } →
            let
              fun = Type.arr (recordApply g) classKeyJSSType.constructor
              inherits' = case inherits of
                Nothing → Type.constructor "React.Basic.Props_div"
                Just t → t
              u = Type.app (Type.constructor "PropsOptions") [ c ]
            in
              constrained "Prim.Row.Union" [ g, r, u] fun
        in
          declValue
            (Ident "component")
            (Expr.app (Expr.ident "React.Basic.element") componentValue.var)
            (Just signature)
    let
      declarations :: List _
      declarations = Array.toUnfoldable
        [ classKeyGenericOptionsType.declaration
        , classKeyOptionsType.declaration
        , classKeyType.declaration
        , classKeyValue.declaration
        , classKeyJSSOptionsType.declaration
        , classKeyJSSType.declaration
        , classKeyJSSValue.declaration
        , componentValue.declaration
        , componentConstructor.declaration
        , componentConstructor'.declaration
        ]
    pure { declarations, prop: Just classKeyType.constructor }
  c → throwError $ Array.singleton $ line
    [ show "classses", "prop is missing or has wrong type:", "_", "in instance object"]
