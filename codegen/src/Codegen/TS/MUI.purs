module Codegen.TS.MUI where

import Prelude

import Codegen.AST (Declaration(..), Ident(..), ModuleName(..), TypeName(..))
import Codegen.AST (Module(..), RowF(..), TypeName(..), Union(..), Type) as AST
import Codegen.AST.Sugar (SListProxy(..), declForeignValue, declType, declValue, forAllValueBinding, local)
import Codegen.AST.Sugar (local) as Sugar
import Codegen.AST.Sugar.Expr (app, ident, ident') as Expr
import Codegen.AST.Sugar.Type (app, arr, constructor, recordLiteral, row, string, symbol, typeRow, typeRow', var) as Type
import Codegen.AST.Sugar.Type (arr) as T
import Codegen.AST.Sugar.Type (constrained, forAll, opt, recordLiteral, recordLiteral')
import Codegen.AST.Types (TypeF(..))
import Codegen.Model (Component, ComponentName, ModulePath, PropsType, componentFullPath, jsImportPath, jsx, psImportPath, reactComponentApply)
import Codegen.Model (componentName) as Model
import Codegen.TS.Module (PossibleType(..), astAlgebra, buildAndInstantiateDeclarations, unionDeclarations) as TS.Module
import Codegen.TS.Module (exprSProxy, exprUnsafeCoerceApp)
import Codegen.TS.Types (InstanceProps, InstantiationStrategy(..), M)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.State (runState)
import Data.Array (elem, filter, fromFoldable, null, singleton, toUnfoldable) as Array
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Mu (Mu(..), roll)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:))
import Data.List (List(..), fromFoldable, singleton) as List
import Data.Map (Map, filterKeys, fromFoldable, keys, lookup, singleton) as Map
import Data.Map.Internal (keys) as Map.Internal
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Set (member) as Set
import Data.String (joinWith)
import Data.String.Extra (camelCase)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Matryoshka (cata, cataM)
import ReadDTS.Instantiation (Property, Type, TypeF(..)) as ReadDTS.Instantiation
import ReadDTS.Instantiation.Pretty (pprintTypeName)
import Record.Extra (type (:::), SNil)

type TsImportPath = String

tsImportPath :: ModulePath -> TsImportPath
tsImportPath modulePath = "@material-ui/core/" <> (jsImportPath modulePath)

propsRowTypeName :: ComponentName -> String
propsRowTypeName componentName = componentName <> "PropsRow"

propsRowOpenTypeName :: ComponentName -> String
propsRowOpenTypeName componentName = componentName <> "PropsRowOpen"


rCons :: AST.Type → AST.Type → AST.Type
rCons h t = Type.app (Type.constructor "MUI.Core.RCons") [ h, t ]

rNil :: AST.Type
rNil = Type.constructor "MUI.Core.RNil"

rList :: Array AST.Type → AST.Type
rList types = foldr step rNil types
  where
    step h l = rCons h l

rList' :: Array String  → AST.Type
rList' = rList <<< map Type.constructor

componentProps ::
  Component ->
  M InstanceProps
componentProps component@{ modulePath } = do
  tsDeclarations <-
    TS.Module.buildAndInstantiateDeclarations
      { path: instanceModulePath
      , source: Just source
      }
  case Map.lookup instanceTypeName tsDeclarations, component.propsType.instantiation of
    Nothing, _ ->
      throwError $ Array.singleton
        $ line
            [ "Unable to find generated props instance type:", show instanceTypeName ]
    Just ds, Just { extractProps } -> except $ extractProps ds.defaultInstance
    Just { defaultInstance: Mu.In (ReadDTS.Instantiation.Object n props), typeConstructor }, _ -> do
      pure { fqn: n, props }
    Just { defaultInstance }, _ ->
      throwError $ Array.singleton
        $ lines
            [ line
                [ "Props instance type"
                , show instanceTypeName
                , "is not an object. Derived type is: "
                , show $ cata pprintTypeName defaultInstance
                ]
            , "Generated ts source code was:"
            , source
            ]
  where
  componentName :: String
  componentName = Model.componentName component

  propsName = componentName <> "Props"

  instanceTypeName = propsName <> "Instance"

  instanceModulePath = instanceTypeName <> ".d.ts"

  -- | This approach is described in `Codegen.Typescript.Module`
  instantiationStrategy = maybe InterfaceInheritance _.strategy component.propsType.instantiation

  source =
    lines
      $ [ line [ "import", "{", propsName, "}", "from", show $ tsImportPath modulePath ]
        -- | Interface extending forces ts type checker to resolve all type fields.
        -- | It won't work with just type aliasing :-(
        , line
            $ case instantiationStrategy of
                InterfaceInheritance -> [ "export interface ", instanceTypeName, "extends", propsName <> " {};" ]
                TypeAlias -> [ "export type ", instanceTypeName, "=", propsName <> ";" ]
        ]

validateProps :: PropsType -> InstanceProps → M Unit
validateProps { base, generate } { props } = do
  let
    missingFromGenerate :: Array String
    missingFromGenerate = Array.filter (not <<< flip Set.member (Map.keys props)) generate

  when (not <<< Array.null $ missingFromGenerate) do
    throwError $ [ "Properties listed for generation but not found in the component props:" <> show missingFromGenerate ]

  let
    propsNamesFromBase :: Array String
    propsNamesFromBase = Array.fromFoldable $ Map.keys base

    missingFromBase :: Array String
    missingFromBase = Array.filter (not <<< flip Set.member (Map.keys props)) propsNamesFromBase

  when (not <<< Array.null $ missingFromBase) do
    throwError $ [ "Properties listed in the base row but not found in the component props:" <> show missingFromBase ]

componentAST :: Component -> M AST.Module
componentAST component@{ extraDeclarations, inherits, modulePath, propsType: expectedProps@{ base, generate } } = do
  instanceProps@{ fqn, props } <- componentProps component
  validateProps expectedProps instanceProps
  let
    -- | These are props which we want to autogenerate
    genProps :: Map.Map String { optional :: Boolean, type :: ReadDTS.Instantiation.Type }
    genProps = Map.filterKeys ((&&) <$> (not <<< eq "classes") <*> (_ `Array.elem` generate)) props

    -- | Create an new "Object" type from them
    -- | for AST generation.
    obj :: ReadDTS.Instantiation.Type
    obj = roll $ ReadDTS.Instantiation.Object fqn genProps

    objInstance :: Tuple (Either String TS.Module.PossibleType) (List AST.Union)
    objInstance = flip runState mempty <<< runExceptT <<< cataM TS.Module.astAlgebra $ obj

    componentName :: String
    componentName = Model.componentName component

  case objInstance of
    Tuple (Right (TS.Module.ProperType (Mu.In (TypeRecord (AST.Row { labels, tail: Nothing }))))) unions -> do
      classes <-
        if "classes" `Array.elem` generate then
          Just <$> classesPropAST componentName (Map.lookup "classes" props)
        else
          pure Nothing
      let

        classesProp :: Map.Map String AST.Type
        classesProp = maybe mempty (Map.singleton "classes" <<< opt <<< _.prop) classes

        componentPropsParam = let ident = Ident "componentProps" in { ident, var: Type.var ident }

        propsRowOpenTypeDecl :: { constructor :: AST.Type, declaration :: Declaration }
        propsRowOpenTypeDecl =
          let
            toOpt (In (TypeOpt ref)) = opt ref
            toOpt ref = ref

            labels' = map toOpt labels
            -- | User doesn't provide optionality info.
            -- | We retrive it from the final type here.
            baseProps = mapWithIndex step base
              where
              step label t = case Map.lookup label labels of
                (Just r) → toOpt r
                otherwise → t

            propsBody :: AST.Type
            propsBody = Type.typeRow $ Type.row (classesProp <> labels' <> baseProps) (Just componentPropsParam.var)
          in
            declType
              (AST.TypeName $ propsRowOpenTypeName componentName)
              [ componentPropsParam.ident ]
              propsBody

        propsRowTypeDecl :: { constructor :: AST.Type, declaration :: Declaration }
        propsRowTypeDecl =
            declType
              (AST.TypeName $ propsRowTypeName componentName)
              [ ]
              (Type.app (propsRowOpenTypeDecl.constructor) [ Type.typeRow' mempty Nothing ])

      (unions' :: List { constructors :: Declaration, instances :: List Declaration, "type" :: Declaration }) <-
        for unions
          $ case _ of
              AST.Union { moduleName: Just _, name } _ ->
                throwError
                  $ [ "External union generation not implmented yet..." ]
              AST.Union { moduleName: Nothing, name } members -> TS.Module.unionDeclarations name members
      let
        step :: { constructors :: Declaration, instances :: List Declaration, "type" :: Declaration } -> List Declaration -> List Declaration
        step { "type": union, constructors, instances } res = List.Cons union (List.Cons constructors res) <> instances

        -- | Our final component module consists of:
        -- | * unions declrations
        -- | * classes realted declarations
        -- | * component constructor + foreign component import
        declarations :: List Declaration
        declarations =
          foldr step List.Nil unions'
            <> List.fromFoldable extraDeclarations
            <> maybe mempty _.declarations classes
            <> List.singleton propsRowOpenTypeDecl.declaration
            <> List.singleton propsRowTypeDecl.declaration
            <> componentConstructorsAST
                { componentName
                , propsRowConstructor: propsRowTypeDecl.constructor
                , propsRowOpenConstructor: propsRowOpenTypeDecl.constructor
                , hasStyles: isJust classes
                , inherits
                }
      pure $ AST.Module
        $ { declarations
          , moduleName: ModuleName $ psImportPath (componentFullPath component)
          }
    (Tuple (Right result) _) ->
      throwError $ Array.singleton $ line
        $ [ "Expecting object type as a result of props instantiation: ", show result ]
    (Tuple (Left err) _) -> throwError [ err ]


componentConstructorsAST ::
  { componentName :: ComponentName
  , propsRowConstructor :: AST.Type
  , propsRowOpenConstructor :: AST.Type
  , hasStyles :: Boolean
  , inherits :: Maybe AST.Type
  } ->
  List Declaration
componentConstructorsAST { componentName, propsRowConstructor, propsRowOpenConstructor, hasStyles, inherits } = constructors
  where
  -- | Maybe this `Writer` here is a bit overkill ;-)
  constructors :: List Declaration
  constructors = do
    let
      -- | For example:
      -- |
      -- | foreign import _UnsafeBadge
      -- |    :: forall componentProps
      -- |     . React.Basic.ReactComponent (BadgeProps componentProps)
      -- |
      unsafeComponentIdent = Ident ("_" <> "Unsafe" <> componentName)
      unsafeComponentDecl = declForeignValue unsafeComponentIdent $ forAll { c: "componentProps" } \{ c } →
        reactComponentApply (recordLiteral (Type.app propsRowOpenConstructor [ c ]))

      defaultRoot = Type.constructor "React.Basic.DOM.Props_div"

      inherits' = fromMaybe (rCons defaultRoot rNil) inherits
      -- | `MergeProps (Tuple Props inherits) p ⇒ t`
      mergeConstraint p t =
        let
          propsChain = rCons propsRowConstructor inherits'
        in
          constrained "MUI.Core.MergeProps" [ propsChain, p ] t

      -- | `Coerce { | i } { | o } ⇒ t`
      coerceConstraint i o t =
        constrained
          "Data.Undefined.NoProblem.Mono.Coerce"
          [ Type.recordLiteral i, Type.recordLiteral o ] t

      rowLacksConstraint l r t = constrained "Prim.Row.Lacks" [ l, r ] t

      -- | For example:
      -- |
      -- | _Badge
      -- |   :: forall props
      -- |   . MergeProps (BadgePropsRow ()) Props_div props
      -- |   ⇒ React.Basic.ReactComponent { | props }
      -- | _Badge = unsafeCoerce _UnsafeBadge
      componentIdent = Ident ("_" <> componentName)
      componentVarExpr = Expr.ident (Sugar.local componentIdent)
      componentDecl = declValue componentIdent [] (exprUnsafeCoerceApp unsafeComponentDecl.var) (Just signature) []
        where
          signature = forAll { p: "props" } \{ p } →
            let
              rc = reactComponentApply (recordLiteral p)
            in
              mergeConstraint p rc

      reactElemApp component props =
        Expr.app (Expr.app (Expr.ident' "React.Basic.element") component) props

      coerceAppExpr value = Expr.app (Expr.ident' "Data.Undefined.NoProblem.Mono.coerce") value

      -- | For example: appBar
      constructorName = camelCase componentName

      propsPartialIdent = Ident "propsPartial"
      propsPartialVar = Expr.ident (Sugar.local propsPartialIdent)

      -- | For example:
      -- |
      -- | badge :: forall propsPartial props
      -- |   . MergeProps (BadgePropsRow ()) Props_div props
      -- |   ⇒ Coerce { | propsPartial } { | props }
      -- |   ⇒ { | propsPartial }
      -- |   → JSX
      -- | badge propsPartial = element _Badge (coerce propsPartial)
      -- |
      constructorDecl = declValue
        constructorIdent
        [ propsPartialIdent ]
        (reactElemApp componentVarExpr (coerceAppExpr propsPartialVar))
        (Just signature)
        []
        where
          constructorIdent = Ident (constructorName)

          signature = forAll { pp: "propsPartial", p: "props" } \{ p, pp } →
            let
              fun = Type.arr (Type.recordLiteral pp) jsx
            in
              mergeConstraint p (coerceConstraint pp p fun)

      -- | I'm using here new sugar for ValueBindings generation.
      -- |
      -- | Generates something like this:
      -- |
      -- | badgeRooted
      -- |   ∷ ∀ componentProps props propsPartial
      -- |   . MergeProps (BadgePropsRow ()) componentProps props
      -- |   ⇒ Row.Lacks "component" props
      -- |   ⇒ Coerce { | propsPartial } { | props }
      -- |   ⇒ ReactComponent { | componentProps }
      -- |   → { | propsPartial }
      -- |   → JSX
      -- | badgeRooted component propsPartial = element _SafeBadge props
      -- |   where
      -- |     _component :: SProxy "component"
      -- |     _component = SProxy
      -- |
      -- |     props = Record.insert _component component (coerce propsPartial)
      -- |
      -- |     _SafeBadge ∷ ReactComponent { component ∷ ReactComponent { | componentProps } | props }
      -- |     _SafeBadge = unsafeCoerce _UnsafeBadge
      -- |
      constructorRooted =
        let
          constructorRootedName = camelCase (componentName <> "Rooted")
        in DeclValue $
          forAllValueBinding
            { cp: "componentProps", pp: "propsPartial", p: "props" }
            constructorRootedName
            (SListProxy ∷ _ ("component" ::: "propsPartial" ::: SNil))
              \{ bindersVars, typeVars } →
                let
                  rootComponent = reactComponentApply (recordLiteral typeVars.cp)

                  signature = Just $
                    mergeConstraint typeVars.p $
                    coerceConstraint typeVars.pp typeVars.p $
                    rowLacksConstraint (Type.symbol "component") typeVars.p $
                    Type.arr
                      (reactComponentApply (recordLiteral typeVars.cp)) $
                      Type.arr
                        (Type.recordLiteral typeVars.pp)
                        jsx

                  safeComponentName = "_Save" <> componentName
                  safeComponent = forAllValueBinding {} (safeComponentName) (SListProxy ∷ _ SNil) \x →
                    { signature: Just $ reactComponentApply (recordLiteral' (Map.singleton "component" rootComponent) typeVars.p)
                    , expr: exprUnsafeCoerceApp (Expr.ident $ local unsafeComponentIdent)
                    , whereBindings: []
                    }

                  sProxy = exprSProxy "component"

                  props = forAllValueBinding {} "props" (SListProxy ∷ _ SNil) \_ →
                    { expr:
                        let
                          p = coerceAppExpr bindersVars.propsPartial
                        in
                          Expr.app (Expr.app (Expr.app (Expr.ident' "Record.insert") sProxy.var) bindersVars.component) p
                    , signature: Nothing
                    , whereBindings: []
                    }

                  expr = reactElemApp (Expr.ident' safeComponentName) (Expr.ident' "props")
                in
                  { signature, whereBindings: [ sProxy.value, props, safeComponent ], expr }

    unsafeComponentDecl.declaration
      : componentDecl.declaration
      : constructorDecl.declaration
      : constructorRooted
      : Nil

-- -- | Generates all declarations related to classes.
-- -- |
-- -- | We are extracting classes directly from AST of a Props object.
-- -- | We could do the same on the generated instance but then we would
-- -- | be forced to remove `classes` from props
-- -- | before running `astAlgebra` on it. Classes record
-- -- | does not translate directly to any expected PS
-- -- | construct because it contains `any` types.
-- -- |
classesPropAST :: ComponentName -> Maybe (ReadDTS.Instantiation.Property ReadDTS.Instantiation.Type) -> M { declarations :: List Declaration, prop :: AST.Type }
classesPropAST componentName = case _ of
  Just { "type": Mu.In (ReadDTS.Instantiation.Object _ classesProps) } -> do
    let
      componentName' = camelCase componentName

      classesNames = Map.Internal.keys classesProps

      binder = Ident "a"

      opt = Type.constructor "Data.Undefined.NoProblem.Opt"
      var = Type.app opt [ roll (TypeVar binder) ]

      -- Construct row type which looks like this:
      -- `type ClassKeyOptions a = ( root :: a, colorPrimary :: a)`
      classesGenericRow =
        roll
          <<< TypeRow
          <<< flip Type.row Nothing
          <<< Map.fromFoldable
          <<< map (flip Tuple var)
          $ classesNames

      classesGenericRowType =
        declType
          (TypeName $ componentName <> "ClassesGenericRow")
          [ binder ]
          classesGenericRow

      undefinedMonoCoerce = Expr.ident' "Data.Undefined.NoProblem.Mono.coerce"

      jss = Type.constructor "MUI.Core.JSS"

      classesKeyType =
        declType
          (TypeName $ componentName <> "ClassesKey")
          []
          (recordLiteral (Type.app classesGenericRowType.constructor [ Type.string ]))

      classesKeyValue =
        let
          ident = Ident $ componentName' <> "ClassesKey"

          signature =
            forAll { g: "given" }
              $ \{ g } ->
                  let
                    fun = T.arr (recordLiteral g) classesKeyType.constructor
                  in
                    constrained "Data.Undefined.NoProblem.Mono.Coerce" [ recordLiteral g, classesKeyType.constructor ] fun
        in
          declValue ident [] undefinedMonoCoerce (Just signature) []

      classesJSSType =
        declType
          (TypeName $ componentName <> "ClassesJSS")
          []
          (recordLiteral (Type.app classesGenericRowType.constructor [ jss ]))

      classesJSSValue =
        let
          ident = Ident $ componentName' <> "ClassesJSS"

          signature =
            forAll { g: "given" }
              $ \{ g } ->
                  let
                    fun = T.arr (recordLiteral g) classesJSSType.constructor
                  in
                    constrained "Data.Undefined.NoProblem.Mono.Coerce" [ recordLiteral g, classesJSSType.constructor ] fun
        in
          declValue ident [] undefinedMonoCoerce (Just signature) []
    let
      declarations :: List _
      declarations =
        Array.toUnfoldable
          [ classesGenericRowType.declaration
          , classesKeyType.declaration
          , classesKeyValue.declaration
          , classesJSSType.declaration
          , classesJSSValue.declaration
          ]
    pure { declarations, prop: classesKeyType.constructor }
  c ->
    throwError $ Array.singleton
      $ line
          [ show "classses", "prop is missing or has wrong type:", "_", "in instance object" ]

line :: Array String -> String
line = joinWith " "

lines :: Array String -> String
lines = joinWith "\n"
