module Codegen.TS.MUI where

import Prelude

import Codegen.AST (Declaration(..), Ident(..), ModuleName(..), TypeName(..))
import Codegen.AST (Module(..), RowF(..), TypeName(..), Union(..), Type) as AST
import Codegen.AST.Sugar (SListProxy(..), declForeignData', declForeignValue, declType, forAllValueBinding)
import Codegen.AST.Sugar (ident, local) as Sugar
import Codegen.AST.Sugar.Expr (app, ident, ident') as Expr
import Codegen.AST.Sugar.Type (app, arr, constructor, recordLiteral, row, string, typeRow, var) as Type
import Codegen.AST.Sugar.Type (constrained, forAll, recordLiteral)
import Codegen.AST.Types (Kind(..), TypeF(..), TypeVarBinding(..), ValueBindingFields(..), Fields)
import Codegen.Component (Component, ComponentName, ModulePath, Props, PropsRow, componentFullPath, effectApply, foldRoot, jsImportPath, jsx, propsCombinedName, propsOptionalName, propsRequiredName, psImportPath, reactComponentApply)
import Codegen.Component (componentName, inputComponentName) as Component
import Codegen.TS.Module (PossibleType(..), astAlgebra, buildAndInstantiateDeclarations, unionDeclarations) as TS.Module
import Codegen.TS.Module (exprUnsafeCoerce, exprUnsafeCoerceApp)
import Codegen.TS.Types (InstanceProps, InstantiationStrategy(..), M)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (runState)
import Data.Array (elem, filter, fromFoldable, null, singleton, toUnfoldable) as Array
import Data.Either (Either(..))
import Data.Filterable (partition)
import Data.Foldable (elem, foldr)
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Mu (Mu(..), roll)
import Data.List (List(..), fromFoldable, singleton) as List
import Data.List (List, (:))
import Data.Map (Map, filter, filterKeys, filterWithKey, fromFoldable, keys, lookup, singleton, toUnfoldable) as Map
import Data.Map.Internal (keys) as Map.Internal
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Set (member) as Set
import Data.String (joinWith)
import Data.String.Extra (camelCase)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Data.Tuple (snd) as Tuple
import Matryoshka (cata, cataM)
import ReadDTS.Instantiation (Property, Type, TypeF(..)) as ReadDTS.Instantiation
import ReadDTS.Instantiation.Pretty (pprintTypeName)
import Record.Extra (type (:::), SNil)

type TsImportPath
  = String

tsImportPath :: ModulePath -> TsImportPath
tsImportPath modulePath = "@material-ui/" <> (jsImportPath modulePath)

-- | Simple helper for polyomrphic row tail buildup.
-- | A parameter ident + tail var type.
rowTail ::
  { ident :: TypeVarBinding
  , var :: Mu TypeF
  }
rowTail =
  let
    label = Ident "r"
  in
    { ident: TypeVarKinded { label, kind: KindRow }, var: Type.var label }

componentProps ::
  Component ->
  M InstanceProps
componentProps component@{ modulePath } = do
  tsDeclarations <-
    TS.Module.buildAndInstantiateDeclarations
      { path: instanceModulePath
      , source: Just source
      }
  case Map.lookup instanceTypeName tsDeclarations, component.propsRow.ts.instantiation of
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
  componentName = Component.inputComponentName component

  propsName = componentName <> "Props"

  instanceTypeName = propsName <> "Instance"

  instanceModulePath = instanceTypeName <> ".d.ts"

  -- | This approach is described in `Codegen.Typescript.Module`
  instantiationStrategy = maybe InterfaceInheritance _.strategy component.propsRow.ts.instantiation

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

validateProps :: PropsRow -> InstanceProps -> M Unit
validateProps { base, generate } { props } = do
  let
    missingFromGenerate :: Array String
    missingFromGenerate = Array.filter (not <<< flip Set.member (Map.keys props)) generate
  when (not <<< Array.null $ missingFromGenerate) do
    throwError $ [ "Properties listed for generation but not found in the component props:" <> show missingFromGenerate ]
  let
    -- | Some properties could be "forced"
    checkedPropsNamesFromBase :: Array String
    checkedPropsNamesFromBase = Array.fromFoldable $ Map.keys $ Map.filter (\{ force } -> isNothing force) base

    missingFromBase :: Array String
    missingFromBase = Array.filter (not <<< flip Set.member (Map.keys props)) checkedPropsNamesFromBase
  when (not <<< Array.null $ missingFromBase) do
    throwError $ [ "Properties listed in the base row but not found in the component props:" <> show missingFromBase ]

componentAST :: Component -> M AST.Module
componentAST component@{ extraDeclarations, root, modulePath, propsRow: expectedProps@{ base, generate, ts } } = do
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
    objInstance =
      map List.fromFoldable
        <<< flip runState mempty
        <<< runExceptT
        <<< flip runReaderT { unionName: ts.unionName }
        <<< cataM TS.Module.astAlgebra
        $ obj

    componentName :: String
    componentName = Component.componentName component
  case objInstance of
    Tuple (Right (TS.Module.ProperType (Mu.In (TypeRecord (AST.Row { labels, tail: Nothing }))))) unions -> do
      classes <-
        if "classes" `Array.elem` generate then
          Just <$> classesPropAST componentName (Map.lookup "classes" props)
        else
          pure Nothing
      let
        classesProp :: Map.Map String AST.Type
        classesProp = maybe mempty (Map.singleton "classes" <<< recordLiteral <<< _.prop) classes

        propsDecls :: Props { constructor :: AST.Type, declaration :: Declaration }
        propsDecls =
          let
            generatedProps = partitionProps (isOpt <<< Tuple.snd) (Map.filterWithKey (\i t -> i `elem` generate) labels)
              where
              isOpt (In (TypeOpt ref)) = true

              isOpt ref = false

            partitionProps :: forall t. (Tuple String t -> Boolean) -> Fields t -> { optional :: Fields t, required :: Fields t }
            partitionProps isOpt ps =
              let
                ({ yes, no } :: { yes :: List _, no :: List _ }) = partition isOpt (Map.toUnfoldable ps)
              in
                { optional: Map.fromFoldable yes, required: Map.fromFoldable no }

            -- | User doesn't provide optionality info.
            -- | We retrive it from the final type here.
            baseProps = { required: map _.type required, optional: map _.type optional }
              where
              step (Tuple label { force }) = case force, Map.lookup label props of
                Just { required: r }, _ -> not r
                _, Just { optional: o } -> o
                _, _ -> false

              { required, optional } = partitionProps step base

            propsRequiredBody :: AST.Type
            propsRequiredBody =
              Type.typeRow
                $ Type.row (generatedProps.required <> baseProps.required) (Just rowTail.var)

            propsOptionalBody :: AST.Type
            propsOptionalBody =
              Type.typeRow
                $ Type.row (classesProp <> generatedProps.optional <> baseProps.optional) (Just rowTail.var)

            optional =
              declType
                (AST.TypeName $ propsOptionalName componentName)
                [ rowTail.ident ]
                propsOptionalBody

            required =
              declType
                (AST.TypeName $ propsRequiredName componentName)
                [ rowTail.ident ]
                propsRequiredBody
          in
            { optional
            , required
            , combined:
                declType
                  (AST.TypeName $ propsCombinedName componentName)
                  [ rowTail.ident ]
                  (Type.app optional.constructor [ Type.app required.constructor [ rowTail.var ] ])
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
        declarations =
          foldr step List.Nil unions'
            <> List.fromFoldable extraDeclarations
            <> maybe mempty _.declarations classes
            <> List.singleton propsDecls.optional.declaration
            <> List.singleton propsDecls.required.declaration
            <> List.singleton propsDecls.combined.declaration
            <> componentConstructorsAST
                { component
                , jss: _.jss <$> classes
                , props:
                    { combined: propsDecls.combined.constructor
                    , optional: propsDecls.optional.constructor
                    , required: propsDecls.required.constructor
                    }
                }
      pure $ AST.Module
        $ { declarations
          , moduleName:
              ModuleName
                $ psImportPath (componentFullPath component.modulePath.output)
          }
    (Tuple (Right result) _) ->
      throwError $ Array.singleton $ line
        $ [ "Expecting object type as a result of props instantiation: ", show result ]
    (Tuple (Left err) _) -> throwError [ err ]

componentConstructorsAST ::
  { component :: Component
  , jss :: Maybe AST.Type
  , props :: Props AST.Type
  } ->
  List Declaration
componentConstructorsAST { component, jss, props } = constructors
  where
  constructors :: List Declaration
  constructors = do
    let
      nubConstraint input nubbed rest = constrained "MUI.Core.Nub'" [ input, nubbed ] rest

      reactElemApp c = Expr.app (Expr.app (Expr.ident' "React.Basic.element") c)

      functorMap = Expr.ident' "Prelude.map"

      muiElem = Expr.ident' "MUI.React.Basic.element"

      muiElemApp c = Expr.app (Expr.app muiElem c)

      componentName = Component.componentName component

      -- | `Coerce { | i } { | o } => t`
      unionConstraint s t u type_ =
        constrained
          "Prim.Row.Union"
          [ s, t, u ]
          type_

      -- | For example:
      -- |
      -- | foreign import _UnsafeBadge
      -- |    :: forall componentProps
      -- |     . React.Basic.ReactComponent (BadgeProps componentProps)
      -- |
      _UnsafeComponentIdent = Ident ("_" <> "Unsafe" <> componentName)

      _UnsafeComponentDecl =
        declForeignValue _UnsafeComponentIdent
          $ forAll { c: "componentProps" } \{ c } ->
              reactComponentApply (recordLiteral (Type.app props.combined [ c ]))

      _Theme = Type.constructor "MUI.Core.Styles.Theme"

      -- | For exmample:
      -- |
      -- | foreign import data Props :: Type
      _Props = declForeignData' (componentName <> "Props")

      decls =
        let
          rootProps = foldRoot { component, local: true }

          -- | _Button
          -- |   :: forall given optionalGiven optionalMissing props required
          -- |   . Nub (ButtonRequiredProps (ButtonBaseRequiredProps ())) required
          -- |   => Union required optionalGiven given
          -- |   => Nub (ButtonProps (ButtonBaseProps Props_div)) props
          -- |   => Union given optionalMissing props
          -- |   => ReactComponent { | given }
          -- | _Button = unsafeCoerce _UnsafeBadge
          _ComponentBinding@(ValueBindingFields { value: { name: _ComponentIdent }}) =
            forAllValueBinding
              (SListProxy :: _ ("given" ::: "optionalGiven" ::: "optionalMissing" ::: "props" ::: "required" ::: SNil))
              ("_" <> componentName)
              (SListProxy :: _ SNil) \{ typeVars } ->
              { signature:
                  Just
                    $ nubConstraint rootProps.required typeVars.required
                    $ unionConstraint typeVars.required typeVars.optionalGiven typeVars.given
                    $ nubConstraint rootProps.combined typeVars.props
                    $ unionConstraint typeVars.given typeVars.optionalMissing typeVars.props
                    $ reactComponentApply (Type.recordLiteral typeVars.given)
              , expr: exprUnsafeCoerceApp _UnsafeComponentDecl.var
              , whereBindings: []
              }

          _WrappedPropsName = camelCase ("props")

          wrappedPropsBinding =
            forAllValueBinding
              (SListProxy :: _ ("given" ::: "optionalGiven" ::: "optionalMissing" ::: "props" ::: "required" ::: SNil))
              _WrappedPropsName
              (SListProxy :: _ SNil) \{ typeVars } ->
              { signature:
                  Just
                    $ nubConstraint rootProps.required typeVars.required
                    $ unionConstraint typeVars.required typeVars.optionalGiven typeVars.given
                    $ nubConstraint rootProps.combined typeVars.props
                    $ unionConstraint typeVars.given typeVars.optionalMissing typeVars.props
                    $ Type.arr
                        (Type.recordLiteral typeVars.given)
                        _Props.constructor
              , expr: exprUnsafeCoerce
              , whereBindings: []
              }

          -- | Component which is defined against wrapped props
          -- | _Button'
          -- |   :: ReactComponent ButtonProps
          -- | _Button' = unsafeCoerce _UnsafeBadge
          _ComponentForWrappedPropsBinding@(ValueBindingFields { value: { name: _ComponentForWrappedPropsIdent } }) =
            ValueBindingFields
              { value:
                  { name: Sugar.ident $ "_" <> componentName <> "'"
                  , binders: []
                  , expr: exprUnsafeCoerceApp _UnsafeComponentDecl.var
                  , whereBindings: []
                  }
              , signature: Just $ reactComponentApply _Props.constructor
              }

          _ComponentForWrappedPropsVar = Expr.ident <<< Sugar.local $ _ComponentForWrappedPropsIdent

          _ComponentVar = Expr.ident <<< Sugar.local $ _ComponentIdent
        in
          { _Component: DeclValue _ComponentBinding
          , _ComponentForWrappedProps: DeclValue _ComponentForWrappedPropsBinding
          -- | For example:
          -- |
          -- | button::forall given optionalGiven optionalMissing props required. 
          -- |   Nub' (ButtonReqPropsRow (MUI.Core.ButtonBase.ButtonBaseReqPropsRow ())) required =>
          -- |   Prim.Row.Union required optionalGiven given =>
          -- |   Nub' (ButtonPropsRow (MUI.Core.ButtonBase.ButtonBasePropsRow React.Basic.DOM.Props_button)) props =>
          -- |   Prim.Row.Union given optionalMissing props =>
          -- |   { | given }  ->  JSX
          -- | button props = element _Button props
          , constructor:
              DeclValue
                $ forAllValueBinding
                    (SListProxy :: _ ("given" ::: "optionalGiven" ::: "optionalMissing" ::: "props" ::: "required" ::: SNil))
                    (camelCase componentName)
                    (SListProxy :: _ ("ps" ::: SNil)) \{ typeVars, vars } ->
                    { signature:
                        Just
                          $ nubConstraint rootProps.required typeVars.required
                          $ unionConstraint typeVars.required typeVars.optionalGiven typeVars.given
                          $ nubConstraint rootProps.combined typeVars.props
                          $ unionConstraint typeVars.given typeVars.optionalMissing typeVars.props
                          $ Type.arr (Type.recordLiteral typeVars.given) jsx
                    -- | This ident could be taken from the above declaration
                    , expr: reactElemApp (Expr.ident' $ "_" <> componentName) vars.ps
                    , whereBindings: []
                    }
          , props: DeclValue wrappedPropsBinding

          -- | Constraint free constructor for example
          -- |
          -- | button' ∷ ButtonProps → JSX
          -- | button' ps = MUI.React.Basic.element _Button' ps
          , constructorFromProps:
              DeclValue
                $ forAllValueBinding
                    (SListProxy ∷ _ SNil)
                    (camelCase componentName <> "'")
                    (SListProxy :: _ SNil) \{ typeVars, vars } ->
                      { signature:
                          Just
                            $ Type.arr _Props.constructor jsx
                      -- | This ident could be taken from the above declaration
                      , expr: Expr.app muiElem _ComponentForWrappedPropsVar
                      , whereBindings: []
                      }
          -- | For example:
          -- |
          -- | standardTextFieldWithStyles ::
          -- |   forall jss jss_. Prim.Row.Union jss jss_ StandardTextFieldClassesJSS =>
          -- |   (MUI.Core.Styles.Theme -> { | jss }) -> StandardTextFieldProps -> JSX
          -- | standardTextFieldWithStyles style =
          -- |   let
          -- |     component = withStyles' style _StandardTextField'
          -- |     withStyles' :: ReactComponent AppBarProps -> Effect.Effect (ReactComponent AppBarProps)
          -- |     withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)
          -- |   in
          -- |     \props -> (unsafeCoerce element) component props
          , constructorWithStyles:
              jss
                <#> \jssType ->
                    DeclValue
                      $ forAllValueBinding
                          (SListProxy :: _ ("jss_" ::: "jss" ::: SNil))
                          (camelCase componentName <> "WithStyles")
                          (SListProxy :: _ ("style" ::: SNil)) \{ typeVars, vars } ->
                          let
                            -- | withStyles' :: (Theme -> Record jss) -> ReactComponent ButtonProps -> ReactComponent ButtonProps
                            -- | withStyles' = unsafeCoerce withStyles
                            withStyles =
                              forAllValueBinding
                                (SListProxy :: _ SNil)
                                ("withStyles'")
                                (SListProxy :: _ SNil) \_ ->
                                { signature:
                                    Just
                                      ( Type.arr
                                          (reactComponentApply _Props.constructor)
                                          (effectApply (reactComponentApply _Props.constructor))
                                      )
                                , expr:
                                    Expr.app
                                      (Expr.ident' "MUI.Core.Styles.withStyles")
                                      (exprUnsafeCoerceApp vars.style)
                                , whereBindings: []
                                }

                            ws@(ValueBindingFields { value: { name: wsIdent } }) = withStyles

                            wsVar = Expr.ident <<< Sugar.local $ wsIdent

                            -- | We want to build this component "up front"
                            -- | component = withStyles' style _Button'
                            styledComponent@(ValueBindingFields { value: { name: styledComponentIdent } }) =
                              forAllValueBinding
                                (SListProxy :: _ SNil)
                                "styledComponent"
                                (SListProxy :: _ SNil) \_ ->
                                { expr: Expr.app wsVar _ComponentForWrappedPropsVar
                                , signature: Nothing
                                , whereBindings: []
                                }

                            styledComponentVar = Expr.ident <<< Sugar.local $ styledComponentIdent

                            render@(ValueBindingFields { value: { name: renderIdent } }) =
                              forAllValueBinding
                                (SListProxy :: _ SNil)
                                "render"
                                (SListProxy :: _ SNil) \{ vars } ->
                                { expr: Expr.app (Expr.app functorMap muiElem) styledComponentVar
                                , signature: Nothing
                                , whereBindings: []
                                }

                            renderVar = Expr.ident <<< Sugar.local $ renderIdent
                          in
                            { signature:
                                Just
                                  $ unionConstraint typeVars.jss typeVars.jss_ jssType
                                  $ Type.arr
                                      ( Type.arr
                                          (Type.constructor "MUI.Core.Styles.Theme")
                                          (Type.recordLiteral typeVars.jss)
                                      )
                                      ( effectApply
                                          $ Type.arr
                                              _Props.constructor
                                              jsx
                                      )
                            , expr: renderVar
                            , whereBindings:
                                [ ws
                                , styledComponent
                                , render
                                ]
                            }
          }
    _UnsafeComponentDecl.declaration
      : decls._Component
      : decls.constructor
      : decls.constructorFromProps
      : decls._ComponentForWrappedProps
      : maybe identity List.Cons decls.constructorWithStyles
          ( _Props.declaration
              : decls.props
              : List.Nil
          )

-- | Generates all declarations related to classes.
-- |
-- | We are extracting classes directly from AST of a Props object.
-- | We could do the same on the generated instance but then we would
-- | be forced to remove `classes` from props
-- | before running `astAlgebra` on it. Classes record
-- | does not translate directly to any expected PS
-- | construct because it contains `any` types.
-- |
classesPropAST ::
  ComponentName ->
  Maybe (ReadDTS.Instantiation.Property ReadDTS.Instantiation.Type) ->
  M
    { declarations :: List Declaration
    , jss :: AST.Type
    , prop :: AST.Type
    }
classesPropAST componentName = case _ of
  Just { "type": Mu.In (ReadDTS.Instantiation.Object _ classesProps) } -> do
    let
      componentName' = camelCase componentName

      classesNames = Map.Internal.keys classesProps

      binderIdent = Ident "a"

      var = roll (TypeVar binderIdent)

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

      undefinedClosedCoerce = Expr.ident' "Data.Undefined.NoProblem.Closed.coerce"

      coerceConstraint given expected rest = constrained "Data.Undefined.NoProblem.Closed.Coerce" [ given, expected ] rest

      jss = Type.constructor "MUI.Core.JSS"

      classesKeyType =
        declType
          (TypeName $ componentName <> "ClassesKey")
          []
          (Type.app classesGenericRowType.constructor [ Type.string ])

      classesJSSType =
        declType
          (TypeName $ componentName <> "ClassesJSS")
          []
          (Type.app classesGenericRowType.constructor [ jss ])
    let
      declarations :: List _
      declarations =
        Array.toUnfoldable
          [ classesGenericRowType.declaration
          , classesKeyType.declaration
          , classesJSSType.declaration
          ]
    pure { declarations, prop: classesKeyType.constructor, jss: classesJSSType.constructor }
  c ->
    throwError $ Array.singleton
      $ line
          [ show "classses", "prop is missing or has wrong type:", "_", "in instance object" ]

line :: Array String -> String
line = joinWith " "

lines :: Array String -> String
lines = joinWith "\n"
