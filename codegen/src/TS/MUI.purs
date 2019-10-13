module Codegen.TS.MUI where

import Prelude

import Codegen.AST (Declaration, Ident(..), ModuleName(..), TypeF(..), TypeName(..))
import Codegen.AST (Module(..), RowF(..), Type, TypeName(..), Union(..), Expr) as AST
import Codegen.AST.Sugar (declForeignData, declForeignValue, declType, declValue)
import Codegen.AST.Sugar.Expr (app, ident) as Expr
import Codegen.AST.Sugar.Type (app, arr, constructor, row, string, typeRow) as Type
import Codegen.AST.Sugar.Type (arr) as T
import Codegen.AST.Sugar.Type (constrained, forAll, forAll', recordApply)
import Codegen.Model (Component, ComponentName, ModulePath, componentFullPath, jsImportPath, psImportPath, reactComponentApply)
import Codegen.Model (componentName, jsx) as Model
import Codegen.TS.Module (PossibleType(..), astAlgebra, declarations, exprUnsafeCoerce, unionDeclarations) as TS.Module
import Codegen.TS.Types (M)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import Data.Array (elem, singleton, toUnfoldable) as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Mu (roll)
import Data.List (List(..), fromFoldable) as List
import Data.List (List)
import Data.Map (Map)
import Data.Map (filterKeys, fromFoldable, lookup, singleton) as Map
import Data.Map.Internal (keys) as Map.Internal
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.String (joinWith)
import Data.String.Extra (camelCase)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Matryoshka (cata, cataM)
import ReadDTS.Instantiation (Property, Type, TypeF(..)) as Instantiation
import ReadDTS.Instantiation.Pretty (pprintTypeName)

type TsImportPath = String

tsImportPath ∷ ModulePath → TsImportPath
tsImportPath modulePath = "@material-ui/core/" <> (jsImportPath modulePath)

propsTypeName ∷ ComponentName → String
propsTypeName componentName = componentName <> "Props"

foreignReactComponentDecl :: ComponentName -> { declaration :: Declaration , ident ∷ Ident, var :: AST.Expr }
foreignReactComponentDecl componentName = { declaration, ident, var }
  where
  ident = Ident ("_" <> componentName)
  { declaration, var } = declForeignValue (ident) (forAll' "a" \a → reactComponentApply [a])

componentProps
  ∷ Component
  → M
    { fqn ∷ String
    , props ∷ Map String (Instantiation.Property Instantiation.Type)
    }
componentProps component@{ modulePath } = do
  tsDeclarations ← TS.Module.declarations
    { path: instanceModulePath
    , source: Just source
    }
  case Map.lookup instanceTypeName tsDeclarations of
    Nothing → throwError $ Array.singleton $ line
      ["Unable to find generated props instance type:", show instanceTypeName ]
    Just { defaultInstance: Mu.In (Instantiation.Object n props), typeConstructor } → do
      pure { fqn: n, props }
    Just { defaultInstance } → throwError $ Array.singleton $ lines
      [ line
        ["Props instance type" , show instanceTypeName
        , "is not an object. Derived type is: ", show $ cata pprintTypeName defaultInstance
        ]
      , "Generated ts source code was:"
      , source
      ]
  where
    componentName = Model.componentName component
    propsName = propsTypeName componentName
    instanceTypeName = propsName <> "Instance"
    instanceModulePath = instanceTypeName <> ".d.ts"
    -- | This approach is described in `Codegen.Typescript.Module`
    source = lines $
      [ line ["import", "{",  propsName, "}", "from", show $ tsImportPath modulePath ]
      -- | Interface extending forces ts type checker to resolve all type fields.
      -- | It won't work with just type aliasing :-(
      , line ["export interface ", instanceTypeName, "extends", propsName <> " {};"]
      ]

componentAST ∷ Component → M AST.Module
componentAST component@{ extraDeclarations, inherits, modulePath, propsType: { base: { row: base, vars }, generate }} = do
  { fqn, props } ← componentProps component
  let
    -- | Take only a subset of props using given label set.
    props' = Map.filterKeys ((&&) <$> (not <<< eq "classes") <*> (_ `Array.elem` generate)) props
    -- | Create an new "Object" type from them
    -- | for AST generation.
    obj ∷ Instantiation.Type
    obj = roll $ Instantiation.Object fqn props'
    objInstance = flip runState mempty <<< runExceptT <<< cataM TS.Module.astAlgebra $ obj

  case objInstance of
    Tuple (Right (TS.Module.ProperType (Mu.In (TypeRecord (AST.Row { labels, tail: Nothing }))))) unions → do
      classes ← if "classes" `Array.elem` generate
        then Just <$> classesPropAST componentName (Map.lookup "classes" props)
        else pure Nothing

      let
        AST.Row base' = base
        classesProp = maybe mempty (Map.singleton "classes" <<< _.prop) classes
        c' = Type.typeRow $ Type.row (classesProp <> labels <> base'.labels) base'.tail
        propsOptionsTypeDecl = declType (AST.TypeName $ propsName <> "Options") vars c'
        propsTypeDecl = declForeignData (AST.TypeName $ propsName)

        propsDeclarations
          = List.Cons (propsOptionsTypeDecl.declaration )
          $ List.Cons (propsTypeDecl.declaration)
          $ List.Nil

      unions' ← for unions $ case _ of
        AST.Union { moduleName: Just _, name } _ → throwError $
          [ "External union generation not implmented yet..." ]
        AST.Union { moduleName: Nothing, name } members →
          pure $ TS.Module.unionDeclarations name members

      let
        step { "type": union, constructors } res =
          List.Cons union (List.Cons constructors res)
        -- | Our final component module consists of:
        -- | * unions declrations
        -- | * classes realted declarations
        -- | * component constructor + foreign component import
        declarations
          = foldr step List.Nil unions'
          <> List.fromFoldable extraDeclarations
          <> propsDeclarations
          <> maybe mempty _.declarations classes
          <> componentConstructorsAST propsOptionsTypeDecl.constructor inherits componentName

      pure $ AST.Module $
        { declarations
        , moduleName: ModuleName $ psImportPath (componentFullPath component)
        }

    (Tuple (Right result) _) → throwError $ Array.singleton $ line $
      ["Expecting object type as a result of props instantiation: " , show result ]
    (Tuple (Left err) _) → throwError [ err ]
  where
    componentName = Model.componentName component
    propsName = propsTypeName componentName

-- | TODO: This is codegen doesn't use typescript AST at all
-- | so we should move it up.
componentConstructorsAST ∷ AST.Type → Maybe AST.Type → ComponentName → List Declaration
componentConstructorsAST propsConstructor inherits componentName =
  let
    componentName' = camelCase componentName
    componentValue = foreignReactComponentDecl componentName
    inherits' = fromMaybe (Type.constructor "React.Basic.DOM.Props_div") inherits

    -- | For example:
    -- | appBar ∷ ∀  given required
    -- |   .  Union given required (AppBarPropsOptions (PaperProps Props_div) )
    -- |   ⇒ Record given
    -- |   → JSX
    -- | appBar = element _AppBar
    componentConstructor =
      let
        signature = forAll { g: "given", r: "required"} $ \{ g, r } →
          let
            fun = Type.arr (recordApply g) Model.jsx
            u = Type.app propsConstructor [ inherits' ]
          in
            constrained "Prim.Row.Union" [ g, r, u] fun
      in
        declValue
          (Ident componentName')
          (Expr.app (Expr.ident "React.Basic.element") componentValue.var)
          (Just signature)

    -- | For example:
    -- | appBar ∷ ∀  componentProps given required
    -- |   .  Union given required (AppBarPropsOptions componentProps)
    -- |   ⇒ Record given
    -- |   → JSX
    -- | appBar = element _AppBar
    componentConstructor' =
      let
        signature = forAll { c: "componentProps", g: "given", r: "required"} $ \{ c, g, r } →
          let
            fun = Type.arr (recordApply g) Model.jsx
            u = Type.app propsConstructor [ c ]
          in
            constrained "Prim.Row.Union" [ g, r, u] fun
      in
        declValue
          (Ident $ componentName' <> "_component")
          (Expr.app (Expr.ident "React.Basic.element") componentValue.var)
          (Just signature)
  in Array.toUnfoldable
    [ componentValue.declaration
    , componentConstructor.declaration
    , componentConstructor'.declaration
    ]

-- | Generates all declarations related to classes.
-- |
-- | We are extracting classes directly from AST of a Props object.
-- | We could do the same on the generated instance but then we would
-- | be forced to remove `classes` from props
-- | before running `astAlgebra` on it. Classes record
-- | does not translate directly to any expected PS
-- | construct because it contains `any` types.
-- |
classesPropAST ∷ ComponentName → Maybe (Instantiation.Property Instantiation.Type) → M { declarations ∷ List Declaration, prop ∷ AST.Type }
classesPropAST componentName = case _ of
  Just { "type": Mu.In (Instantiation.Object _ classesProps) } → do
    let
      componentName' = camelCase componentName
      classesNames = Map.Internal.keys classesProps
      binder = Ident "a"
      var = roll $ TypeVar binder
      -- Construct row type which looks like this:
      -- `type ClassKeyOptions a = ( root ∷ a, colorPrimary ∷ a)`

      classesGenericOptionsRow
        = roll
        <<< TypeRow
        <<< flip Type.row Nothing
        <<< Map.fromFoldable
        <<< map (flip Tuple var)
        $ classesNames

      classKeyGenericOptionsType = declType
        (TypeName $ componentName <> "ClassKeyGenericOptions")
        [ binder ]
        classesGenericOptionsRow

      classKeyJSSOptionsType = declType
        (TypeName $ componentName <> "ClassKeyOptionsJSS")
        []
        (Type.app classKeyGenericOptionsType.constructor [ Type.constructor "MUI.Core.JSS" ])

      classKeyOptionsType = declType
        (TypeName $ componentName <> "ClassKeyOptions")
        []
        (Type.app classKeyGenericOptionsType.constructor [ Type.string ])

      -- | Construct a type and related constructor function which which looks like this:
      -- | ```
      -- | foreign import data BadgeClassKey
      -- |
      -- | classKey ∷ ∀  given required
      -- |  .  Union given required (BadgeClassKeyOptions )
      -- |  ⇒ Record given
      -- |  → BadgeClassKey
      -- | classKey = unsafeCoerce
      -- | ```
      classKeyType = declForeignData (TypeName $ componentName <> "ClassKey")
      classKeyValue =
        let
          ident = Ident $ componentName' <> "ClassKey"
          signature = forAll { g: "given", r: "required"} $ \{ g, r } →
            let
              fun = T.arr (recordApply g) classKeyType.constructor
            in
              constrained "Prim.Row.Union" [ g, r, classKeyOptionsType.constructor ] fun
        in
          declValue ident TS.Module.exprUnsafeCoerce (Just signature)

      classKeyJSSType = declForeignData (TypeName $ componentName <> "ClassKeyJSS")
      classKeyJSSValue =
        let
          ident = Ident $ componentName' <> "ClassKeyJSS"
          signature = forAll { g: "given", r: "required"} $ \{ g, r } →
            let
              fun = T.arr (recordApply g) classKeyJSSType.constructor
            in
              constrained "Prim.Row.Union" [ g, r, classKeyJSSOptionsType.constructor ] fun
        in
          declValue ident TS.Module.exprUnsafeCoerce (Just signature)

    let
      declarations ∷ List _
      declarations = Array.toUnfoldable
        [ classKeyGenericOptionsType.declaration
        , classKeyOptionsType.declaration
        , classKeyType.declaration
        , classKeyValue.declaration
        , classKeyJSSOptionsType.declaration
        , classKeyJSSType.declaration
        , classKeyJSSValue.declaration
        ]
    pure { declarations, prop: classKeyType.constructor }
  c → throwError $ Array.singleton $ line
    [ show "classses", "prop is missing or has wrong type:", "_", "in instance object"]

line ∷ Array String → String
line = joinWith " "

lines ∷ Array String → String
lines = joinWith "\n"

