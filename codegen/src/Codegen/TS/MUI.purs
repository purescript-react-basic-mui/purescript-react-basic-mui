module Codegen.TS.MUI where

import Prelude

import Codegen.AST (Declaration(..), Ident(..), ModuleName(..), TypeName(..))
import Codegen.AST (Module(..), RowF(..), TypeName(..), Union(..), Type) as AST
import Codegen.AST.Sugar (SListProxy(..), declForeignData', declForeignValue, declType, declValue, forAllValueBinding)
import Codegen.AST.Sugar.Expr (app, ident') as Expr
import Codegen.AST.Sugar.Type (app, arr, constructor, recordLiteral, row, string, typeRow, var) as Type
import Codegen.AST.Sugar.Type (arr) as T
import Codegen.AST.Sugar.Type (constrained, forAll, opt, recordLiteral)
import Codegen.AST.Types (Kind(..), TypeF(..), TypeVarBinding(..))
import Codegen.Model (Component, ComponentName, ModulePath, Props, PropsRow, Root(..), componentFullPath, foldRoot, jsImportPath, jsx, propsCombinedName, propsOptionalName, propsRequiredName, psImportPath, reactComponentApply)
import Codegen.Model (componentName, inputComponentName) as Model
import Codegen.TS.Module (PossibleType(..), astAlgebra, buildAndInstantiateDeclarations, unionDeclarations) as TS.Module
import Codegen.TS.Module (exprUnsafeCoerce, exprUnsafeCoerceApp)
import Codegen.TS.Types (InstanceProps, InstantiationStrategy(..), M)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.State (runState)
import Data.Array (elem, filter, fromFoldable, null, singleton, toUnfoldable) as Array
import Data.Either (Either(..))
import Data.Filterable (partition)
import Data.Foldable (elem, foldr)
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Mu (Mu(..), roll)
import Data.List (List(..), fromFoldable, singleton) as List
import Data.List (List, (:))
import Data.Map (Map, filterKeys, filterWithKey, fromFoldable, keys, lookup, singleton, toUnfoldable) as Map
import Data.Map.Internal (keys) as Map.Internal
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.Set (member) as Set
import Data.String (joinWith)
import Data.String.Extra (camelCase)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Tuple (snd) as Tuple
import Debug.Trace (traceM)
import Matryoshka (cata, cataM)
import ReadDTS.Instantiation (Property, Type, TypeF(..)) as ReadDTS.Instantiation
import ReadDTS.Instantiation.Pretty (pprintTypeName)
import Record.Extra (type (:::), SNil)

type TsImportPath = String

tsImportPath :: ModulePath -> TsImportPath
tsImportPath modulePath = "@material-ui/core/" <> (jsImportPath modulePath)

-- | Simple helper for polyomrphic row tail buildup.
-- | A parameter ident + tail var type.
rowTail ::
  { ident :: TypeVarBinding
  , var :: Mu TypeF
  }
rowTail =
  let label = Ident "r"
  in { ident: TypeVarKinded { label, kind: KindRow }, var: Type.var label }

componentProps ::
  Component ->
  M InstanceProps
componentProps component@{ modulePath } = do
  tsDeclarations <-
    TS.Module.buildAndInstantiateDeclarations
      { path: instanceModulePath
      , source: Just source
      }
  case Map.lookup instanceTypeName tsDeclarations, component.propsRow.instantiation of
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
  componentName = Model.inputComponentName component

  propsName = componentName <> "Props"

  instanceTypeName = propsName <> "Instance"

  instanceModulePath = instanceTypeName <> ".d.ts"

  -- | This approach is described in `Codegen.Typescript.Module`
  instantiationStrategy = maybe InterfaceInheritance _.strategy component.propsRow.instantiation

  source =
    lines
      $ [ line [ "import", "{", propsName, "}", "from", show $ tsImportPath modulePath.input ]
        -- | Interface extending forces ts type checker to resolve all type fields.
        -- | It won't work with just type aliasing :-(
        , line
            $ case instantiationStrategy of
                InterfaceInheritance -> [ "export interface ", instanceTypeName, "extends", propsName <> " {};" ]
                TypeAlias -> [ "export type ", instanceTypeName, "=", propsName <> ";" ]
        ]
  x = do
    traceM source
    Nothing

validateProps :: PropsRow -> InstanceProps -> M Unit
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
componentAST component@{ extraDeclarations, root, modulePath, propsRow: expectedProps@{ base, generate } } = do
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

        propsDecls :: Props { constructor :: AST.Type, declaration :: Declaration }
        propsDecls =
          let
            partitionProps isOpt ps =
              let
                ({ yes, no } :: { yes :: List _, no :: List _ }) =
                  partition isOpt (Map.toUnfoldable ps)
              in
                { optional: Map.fromFoldable yes, required: Map.fromFoldable no }

            generatedProps = partitionProps (isOpt <<< Tuple.snd) (Map.filterWithKey (\i t -> i `elem` generate) labels)
              where
                isOpt (In (TypeOpt ref)) = true
                isOpt ref = false

            -- | User doesn't provide optionality info.
            -- | We retrive it from the final type here.
            baseProps = partitionProps step base
              where
              step (Tuple label t) = case Map.lookup label props of
                Just { optional: o } -> o
                otherwise -> false

            propsRequiredBody :: AST.Type
            propsRequiredBody = Type.typeRow $
              Type.row (generatedProps.required <> baseProps.required) (Just rowTail.var)

            propsOptionalBody :: AST.Type
            propsOptionalBody = Type.typeRow $
              Type.row (classesProp <> generatedProps.optional <> baseProps.optional) (Just rowTail.var)

            optional = declType
                (AST.TypeName $ propsOptionalName componentName)
                [ rowTail.ident ]
                propsOptionalBody

            required =  declType
                (AST.TypeName $ propsRequiredName componentName)
                [ rowTail.ident ]
                propsRequiredBody
          in
            { optional
            , required
            , combined: declType
                (AST.TypeName $ propsCombinedName componentName)
                [ rowTail.ident ]
                (Type.app optional.constructor [ Type.app required.constructor [ rowTail.var ]])
            }

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
        declarations
          = foldr step List.Nil unions'
          <> List.fromFoldable extraDeclarations
          <> maybe mempty _.declarations classes
          <> List.singleton propsDecls.optional.declaration
          <> List.singleton propsDecls.required.declaration
          <> List.singleton propsDecls.combined.declaration
          <> componentConstructorsAST
                 { component
                 , props:
                    { combined: propsDecls.combined.constructor
                    , optional: propsDecls.optional.constructor
                    , required: propsDecls.required.constructor
                    }
                 , hasStyles: isJust classes
                 }
      pure $ AST.Module
        $ { declarations
          , moduleName: ModuleName $
              psImportPath (componentFullPath component.modulePath.output)
          }
    (Tuple (Right result) _) ->
      throwError $ Array.singleton $ line
        $ [ "Expecting object type as a result of props instantiation: ", show result ]
    (Tuple (Left err) _) -> throwError [ err ]


componentConstructorsAST ::
  { component :: Component
  , props :: Props AST.Type
  , hasStyles :: Boolean
  } ->
  List Declaration
componentConstructorsAST { component, props } = constructors
  where
  -- | Maybe this `Writer` here is a bit overkill ;-)
  constructors :: List Declaration
  constructors = do
    let
      nubConstraint input nubbed rest = constrained "MUI.Core.Nub'" [ input, nubbed ] rest

      reactElemApp component = Expr.app (Expr.app (Expr.ident' "React.Basic.element") component)
      componentName = Model.componentName component

      -- | `Coerce { | i } { | o } => t`
      unionConstraint s t u type_ =
        constrained
          "Prim.Row.Union"
          [ s, t, u] type_

      -- | For example:
      -- |
      -- | foreign import _UnsafeBadge
      -- |    :: forall componentProps
      -- |     . React.Basic.ReactComponent (BadgeProps componentProps)
      -- |
      _UnsafeComponentIdent = Ident ("_" <> "Unsafe" <> componentName)
      _UnsafeComponentDecl = declForeignValue _UnsafeComponentIdent $ forAll { c: "componentProps" } \{ c } ->
        reactComponentApply (recordLiteral (Type.app props.combined [ c ]))

      -- | For exmample:
      -- |
      -- | foreign import data Props :: Type
      _Props = declForeignData' (componentName <> "Props")

      decls = case component.root of
        -- | When we have root directly from _react-basic_ 
        -- | we have simpler signatures - we dont' have
        -- | to `Nub` required props
        -- |
        RBProps t ->
          -- | For example:
          -- |
          -- | _Badge
          -- |   :: forall given optionalGiven optionalMissing props required
          -- |   . Nub' (BadgeProps Props_div) props
          -- |   => Union given optionalMissing props
          -- |   => ReactComponent { | BadgeRequiredProps given }
          -- | _Badge = unsafeCoerce _UnsafeBadge
          { _Component: DeclValue $ forAllValueBinding
              (SListProxy :: _ ("given" ::: "optionalMissing" ::: "props" ::: SNil))
              ("_" <> componentName)
              (SListProxy :: _ SNil)
              \{ typeVars } ->
                { signature: Just $
                    nubConstraint (Type.app props.combined [ t ]) typeVars.props $
                    unionConstraint typeVars.given typeVars.optionalMissing typeVars.props $
                    reactComponentApply (Type.recordLiteral (Type.app props.required [ typeVars.given ]))
                , expr: exprUnsafeCoerceApp _UnsafeComponentDecl.var
                , whereBindings: []
                }
          -- badge :: forall given optionalGiven optionalMissing props required
          --   . Nub' (BadgeProps Props_div)) props
          --   => Union given optionalMissing props
          --   => { | BadgeRequiredProps given } -> JSX
          -- badge = element (_Badge :: ReactComponent { | BadgeRequiredProps given })
          , constructor: DeclValue $ forAllValueBinding
              (SListProxy :: _ ("given" ::: "optionalMissing" ::: "props" ::: SNil))
              (camelCase componentName)
              (SListProxy :: _ ("props" ::: SNil))
              \{ typeVars, vars } ->
                { signature: Just $
                    nubConstraint (Type.app props.combined [ t ]) typeVars.props $
                    unionConstraint typeVars.given typeVars.optionalMissing typeVars.props $
                    Type.arr
                      (Type.recordLiteral (Type.app props.required [ typeVars.given ]))
                      jsx
                -- | This ident could be taken from the above declaration
                , expr: reactElemApp (Expr.ident' $ "_" <> componentName) vars.props
                , whereBindings: []
                }
          -- | For example:
          -- |
          -- | opaqueProps
          -- |   :: forall given optionalGiven optionalMissing props required
          -- |   . Nub' (BadgeProps Props_div)) props
          -- |   => Union given optionalMissing props
          -- |   => { | BadgeRequiredProps given } -> Props
          , props: DeclValue $ forAllValueBinding
              (SListProxy :: _ ("given" ::: "optionalMissing" ::: "props" ::: SNil))
              (camelCase (componentName <> "Props"))
              (SListProxy :: _ SNil)
              \{ typeVars } ->
                { signature: Just $
                    nubConstraint (Type.app props.combined [ t ]) typeVars.props $
                    unionConstraint typeVars.given typeVars.optionalMissing typeVars.props $
                    Type.arr
                      (Type.recordLiteral (Type.app props.required [ typeVars.given ]))
                      _Props.constructor
                -- | This ident could be taken from the above declaration
                , expr: exprUnsafeCoerce
                , whereBindings: []
                }
          }
        MUIComponent c ->
          let
            rootProps = foldRoot { component, local: true }
          in
            -- _Button
            --   :: forall given optionalGiven optionalMissing props required
            --   . Nub (ButtonRequiredProps (ButtonBaseRequiredProps ())) required
            --   => Union required optionalGiven given
            --   => Nub (ButtonProps (ButtonBaseProps Props_div)) props
            --   => Union given optionalMissing props
            --   => ReactComponent { | given }
            -- _Button = unsafeCoerce _UnsafeBadge
            { _Component: DeclValue $ forAllValueBinding
                (SListProxy :: _ ("given" ::: "optionalGiven" ::: "optionalMissing" ::: "props" ::: "required" ::: SNil))
                ("_" <> componentName)
                (SListProxy :: _ SNil)
                \{ typeVars } ->
                  { signature: Just $
                      nubConstraint rootProps.required typeVars.required $
                      unionConstraint typeVars.required typeVars.optionalGiven typeVars.given $
                      nubConstraint rootProps.combined typeVars.props $
                      unionConstraint typeVars.given typeVars.optionalMissing typeVars.props $
                      reactComponentApply (Type.recordLiteral typeVars.given)
                  , expr: exprUnsafeCoerceApp _UnsafeComponentDecl.var
                  , whereBindings: []
                  }
            , constructor: DeclValue $ forAllValueBinding
                (SListProxy :: _ ("given" ::: "optionalGiven" ::: "optionalMissing" ::: "props" ::: "required" ::: SNil))
                (camelCase componentName)
                (SListProxy :: _ ("props" ::: SNil))
                \{ typeVars, vars } ->
                  { signature: Just $
                      nubConstraint rootProps.required typeVars.required $
                      unionConstraint typeVars.required typeVars.optionalGiven typeVars.given $
                      nubConstraint rootProps.combined typeVars.props $
                      unionConstraint typeVars.given typeVars.optionalMissing typeVars.props $
                      Type.arr (Type.recordLiteral typeVars.given) jsx
                  -- | This ident could be taken from the above declaration
                  , expr: reactElemApp (Expr.ident' $ "_" <> componentName) vars.props
                  , whereBindings: []
                  }
            -- | For example:
            -- |
            -- | props
            -- |   :: forall given optionalGiven optionalMissing props required
            -- |   . Nub' (BadgeProps Props_div)) props
            -- |   => Union given optionalMissing props
            -- |   => { | BadgeRequiredProps given } -> Props
            , props: DeclValue $ forAllValueBinding
                (SListProxy :: _ ("given" ::: "optionalGiven" ::: "optionalMissing" ::: "props" ::: "required" ::: SNil))
                (camelCase (componentName <> "Props"))
                (SListProxy :: _ SNil)
                \{ typeVars } ->
                  { signature: Just $
                      nubConstraint rootProps.required typeVars.required $
                      unionConstraint typeVars.required typeVars.optionalGiven typeVars.given $
                      nubConstraint rootProps.combined typeVars.props $
                      unionConstraint typeVars.given typeVars.optionalMissing typeVars.props $
                      Type.arr
                        (Type.recordLiteral (Type.app props.required [ typeVars.given ]))
                        _Props.constructor
                  -- | TODO
                  --   Just $
                  --     nubConstraint [ props.combined, t ] typeVars.props $
                  --     unionConstraint typeVars.given typeVars.optionalMissing typeVars.props $
                  --     Type.arr
                  --       (Type.recordLiteral (Type.app props.required [ typeVars.given ]))
                  --       _Props.constructor
                  -- | This ident could be taken from the above declaration
                  , expr: exprUnsafeCoerce
                  , whereBindings: []
                  }
            }

    _UnsafeComponentDecl.declaration
      : decls._Component
      : decls.constructor
      : _Props.declaration
      : decls.props
      : List.Nil


      -- 


      -- -- | For example:
      -- -- |
      -- -- | _Badge
      -- -- |   :: forall props
      -- -- |   . MergeProps (BadgePropsRow ()) Props_div props
      -- -- |   => React.Basic.ReactComponent { | props }
      -- -- | _Badge = unsafeCoerce _UnsafeBadge
      -- componentIdent = Ident ("_" <> componentName)
      -- componentVarExpr = Expr.ident (Sugar.local componentIdent)
      -- componentDecl = declValue componentIdent [] (exprUnsafeCoerceApp unsafeComponentDecl.var) (Just signature) []
      --   where
      --     signature = forAll { p: "props" } \{ p } ->
      --       let
      --         rc = reactComponentApply (recordLiteral p)
      --       in
      --         mergeConstraint p rc


      -- coerceAppExpr value = Expr.app (Expr.ident' "Data.Undefined.NoProblem.Mono.coerce") value

      -- -- | For example: appBar
      -- constructorName = camelCase componentName

      -- partialPropsIdent = Ident "partialProps"
      -- partialPropsVar = Expr.ident (Sugar.local partialPropsIdent)

      -- -- | For example:
      -- -- |
      -- -- | badge :: forall partialProps props
      -- -- |   . MergeProps (BadgePropsRow ()) Props_div props
      -- -- |   => Coerce { | partialProps } { | props }
      -- -- |   => { | partialProps }
      -- -- |   -> JSX
      -- -- | badge partialProps = element _Badge (coerce partialProps)
      -- -- |
      -- constructorDecl = declValue
      --   constructorIdent
      --   [ partialPropsIdent ]
      --   (reactElemApp componentVarExpr (coerceAppExpr partialPropsVar))
      --   (Just signature)
      --   []
      --   where
      --     constructorIdent = Ident (constructorName)

      --     signature = forAll { pp: "partialProps", p: "props" } \{ p, pp } ->
      --       let
      --         fun = Type.arr (Type.recordLiteral pp) jsx
      --       in
      --         mergeConstraint p (coerceConstraint pp p fun)

      -- -- | I'm using here new sugar for ValueBindings generation.
      -- -- |
      -- -- | Generates something like this:
      -- -- |
      -- -- | badgeRooted
      -- -- |   :: forall componentProps props partialProps
      -- -- |   . MergeProps (BadgePropsRow ()) componentProps props
      -- -- |   => Row.Lacks "component" props
      -- -- |   => Coerce { | partialProps } { | props }
      -- -- |   => ReactComponent { | componentProps }
      -- -- |   -> { | partialProps }
      -- -- |   -> JSX
      -- -- | badgeRooted component partialProps = element _SafeBadge props
      -- -- |   where
      -- -- |     _component :: SProxy "component"
      -- -- |     _component = SProxy
      -- -- |
      -- -- |     props = Record.insert _component component (coerce partialProps)
      -- -- |
      -- -- |     _SafeBadge :: ReactComponent { component :: ReactComponent { | componentProps } | props }
      -- -- |     _SafeBadge = unsafeCoerce _UnsafeBadge
      -- -- |
      -- constructorRooted =
      --   let
      --     constructorRootedName = camelCase (componentName <> "Rooted")
      --   in DeclValue $
      --     forAllValueBinding
      --       { cp: "componentProps", pp: "partialProps", p: "props" }
      --       constructorRootedName
      --       (SListProxy :: _ ("component" ::: "partialProps" ::: SNil))
      --         \{ bindersVars, typeVars } ->
      --           let
      --             rootComponent = reactComponentApply (recordLiteral typeVars.cp)

      --             signature = Just $
      --               mergeConstraint typeVars.p $
      --               coerceConstraint typeVars.pp typeVars.p $
      --               rowLacksConstraint (Type.symbol "component") typeVars.p $
      --               Type.arr
      --                 (reactComponentApply (recordLiteral typeVars.cp)) $
      --                 Type.arr
      --                   (Type.recordLiteral typeVars.pp)
      --                   jsx

      --             safeComponentName = "_Safe" <> componentName
      --             safeComponent = forAllValueBinding {} (safeComponentName) (SListProxy :: _ SNil) \x ->
      --               { signature: Just $ reactComponentApply (recordLiteral' (Map.singleton "component" rootComponent) typeVars.p)
      --               , expr: exprUnsafeCoerceApp (Expr.ident $ local unsafeComponentIdent)
      --               , whereBindings: []
      --               }

      --             sProxy = exprSProxy "component"

      --             props = forAllValueBinding {} "props" (SListProxy :: _ SNil) \_ ->
      --               { expr:
      --                   let
      --                     p = coerceAppExpr bindersVars.partialProps
      --                   in
      --                     Expr.app (Expr.app (Expr.app (Expr.ident' "Record.insert") sProxy.var) bindersVars.component) p
      --               , signature: Nothing
      --               , whereBindings: []
      --               }

      --             expr = reactElemApp (Expr.ident' safeComponentName) (Expr.ident' "props")
      --           in
      --             { signature, whereBindings: [ sProxy.value, props, safeComponent ], expr }

      -- -- | Building two declarations related to opaque `*Props` type.
      -- -- | For example:
      -- -- |
      -- -- | ```purescript
      -- -- | foreign import data InputProps :: Type
      -- -- |
      -- -- | inputProps ::
      -- -- |   forall props partialProps.
      -- -- |   MergeProps (RCons InputPropsRow (RCons MUI.Core.InputBase.InputBasePropsRow (RCons DOM.Props_div RNil))) props =>
      -- -- |   Coerce { | partialProps } { | props } =>
      -- -- |   { | partialProps } -> InputProps
      -- -- | inputProps partialProps = toProps (coerce partialProps)
      -- -- |   where
      -- -- |     toProps :: { | props } -> InputProps
      -- -- |     toProps = unsafeCoerce
      -- -- | ```
      -- opaquePropsName = componentName <> "OpaqueProps"

    -- unsafeComponentDecl.declaration
    --   : componentDecl.declaration
    --   : constructorDecl.declaration
    --   : constructorRooted
    --   : _OpaqueProps.declaration
    --   : opaqueProps
    --   : Nil

-- | Generates all declarations related to classes.
-- |
-- | We are extracting classes directly from AST of a Props object.
-- | We could do the same on the generated instance but then we would
-- | be forced to remove `classes` from props
-- | before running `astAlgebra` on it. Classes record
-- | does not translate directly to any expected PS
-- | construct because it contains `any` types.
-- |
classesPropAST :: ComponentName -> Maybe (ReadDTS.Instantiation.Property ReadDTS.Instantiation.Type) -> M { declarations :: List Declaration, prop :: AST.Type }
classesPropAST componentName = case _ of
  Just { "type": Mu.In (ReadDTS.Instantiation.Object _ classesProps) } -> do
    let
      componentName' = camelCase componentName

      classesNames = Map.Internal.keys classesProps

      binderIdent = Ident "a"

      opt = Type.constructor "Data.Undefined.NoProblem.Opt"
      var = Type.app opt [ roll (TypeVar binderIdent) ]

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
          [ TypeVarName binderIdent ]
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
