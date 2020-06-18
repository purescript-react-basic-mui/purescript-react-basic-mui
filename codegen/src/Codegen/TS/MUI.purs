module Codegen.TS.MUI where

import Prelude

import Codegen.AST (Declaration(..), Ident(..), ModuleName(..), TypeF(..), TypeName(..))
import Codegen.AST (Expr, Module(..), RowF(..), Type, TypeName(..), Union(..)) as AST
import Codegen.AST.Sugar (SListProxy(..), declForeignValue, declType, declValue, forAllValueBinding, local, qualifiedIdent)
import Codegen.AST.Sugar (local) as Sugar
import Codegen.AST.Sugar.Expr (app, ident, ident') as Expr
import Codegen.AST.Sugar.Type (app, arr, constructor, recordApply, recordLiteral, row, string, symbol, typeRow, var) as Type
import Codegen.AST.Sugar.Type (arr) as T
import Codegen.AST.Sugar.Type (constrained, forAll, forAll', opt, recordLiteral, recordLiteral', typeRow, typeRow')
import Codegen.AST.Types (ExprF(..), TypeF(..))
import Codegen.Model (Component, ComponentName, ModulePath, PropsType, componentFullPath, componentName, jsImportPath, jsx, psImportPath, reactComponentApply)
import Codegen.Model (componentName) as Model
import Codegen.TS.Module (PossibleType(..), astAlgebra, buildAndInstantiateDeclarations, unionDeclarations) as TS.Module
import Codegen.TS.Module (exprSProxy, exprUnsafeCoerce, exprUnsafeCoerceApp)
import Codegen.TS.Types (InstanceProps, InstantiationStrategy(..), M)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (except, runExceptT)
import Control.Monad.State (runState)
import Control.Monad.Writer (execWriter, tell)
import Data.Array (cons, elem, filter, fromFoldable, null, singleton, tail, toUnfoldable) as Array
import Data.Array.NonEmpty (cons') as NonEmptyArray
import Data.Either (Either(..))
import Data.Foldable (foldr)
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Mu (Mu(..), roll)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), fromFoldable, singleton) as List
import Data.List (List)
import Data.Map (Map, filterKeys, fromFoldable, keys, lookup, singleton) as Map
import Data.Map.Internal (keys) as Map.Internal
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Set (member) as Set
import Data.String (joinWith)
import Data.String.Extra (camelCase)
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Matryoshka (cata, cataM)
import Node.FS (FileFlags(..))
import ReadDTS.Instantiation (Property, Type, TypeF(..)) as ReadDTS.Instantiation
import ReadDTS.Instantiation.Pretty (pprintTypeName)
import Record.Extra (type (:::), SNil)
import Unsafe.Coerce (unsafeCoerce)

type TsImportPath = String

tsImportPath :: ModulePath -> TsImportPath
tsImportPath modulePath = "@material-ui/core/" <> (jsImportPath modulePath)

propsTypeName :: ComponentName -> String
propsTypeName componentName = componentName <> "Props"

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

  propsName = propsTypeName componentName

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

    propsName :: String
    propsName = propsTypeName componentName

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

        propsRowTypeDecl :: { constructor :: AST.Type, declaration :: Declaration }
        propsRowTypeDecl =
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
            declType (AST.TypeName $ propsName <> "Row") [ componentPropsParam.ident ] propsBody

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
            <> List.singleton propsRowTypeDecl.declaration
            <> componentConstructorsAST
                { componentName
                , propsRowConstructor: propsRowTypeDecl.constructor
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



-- |
-- | foreign import _UnsafeBadge :: forall componentProps. React.Basic.ReactComponent (BadgeProps componentProps)
-- |
-- | _Badge
-- |   :: forall props
-- |   . MergeProps (BadgePropsRow ()) Props_div props
-- |   ⇒ React.Basic.ReactComponent { | props }
-- | _Badge = unsafeCoerce _UnsafeBadge
-- |
-- | badge :: forall partialProps props
-- |   . MergeProps (BadgePropsRow ()) Props_div props
-- |   ⇒ Coerce { | partialProps } { | props }
-- |   ⇒ { | partialProps }
-- |   → JSX
-- | badge propsPartial = element _Badge (coerce propsPartial)
-- |
-- | badge'
-- |   ∷ ∀ componentProps props propsPartial
-- |   . MergeProps (BadgePropsRow ()) componentProps props
-- |   ⇒ Row.Lacks "component" props
-- |   ⇒ Coerce { | propsPartial } { | props }
-- |   ⇒ ReactComponent { | componentProps }
-- |   → { | propsPartial }
-- |   → JSX
-- | badge' component propsPartial =
-- |   let
-- |     props = Record.insert (SProxy ∷ SProxy "component") component (coerce propsPartial)
-- |
-- |     _SafeBadge ∷ ReactComponent { component ∷ ReactComponent { | componentProps } | props }
-- |     _SafeBadge = unsafeCoerce _UnsafeBadge
-- |   in
-- |     element _SafeBadge props
-- |
componentConstructorsAST ::
  { componentName :: ComponentName
  , propsRowConstructor :: AST.Type
  , hasStyles :: Boolean
  , inherits :: Maybe AST.Type
  } ->
  List Declaration
componentConstructorsAST { componentName, propsRowConstructor, hasStyles, inherits } = constructors
  where
  -- | Maybe this `Writer` here is a bit overkill ;-)
  constructors :: List Declaration
  constructors = execWriter do
    let
      -- | For example:
      -- |
      -- | foreign import _UnsafeBadge
      -- |    :: forall componentProps
      -- |     . React.Basic.ReactComponent (BadgeProps componentProps)
      -- |
      unsafeComponentIdent = Ident ("_" <> "Unsafe" <> componentName)
      unsafeComponentDecl = declForeignValue unsafeComponentIdent $ forAll { c: "componentProps" } \{ c } →
        reactComponentApply (recordLiteral (Type.app propsRowConstructor [ c ]))

      -- | `MergeProps Props inherits p ⇒ t`
      mergeConstraint p t =
        let
          propsRow = Type.app propsRowConstructor [ typeRow' mempty Nothing ]
        in
          constrained "MUI.Core.MergeProps" [ propsRow, inherits', p ] t

      -- | `Coerce { | i } { | o } ⇒ t`
      coerceConstraint i o t =
        let
          propsRow = Type.app propsRowConstructor [ typeRow' mempty Nothing ]
        in
          constrained
            "Data.Undefined.NoProblem.Mono.Coerce"
            [ Type.recordLiteral i, Type.recordLiteral o ] t

      rowLacksConstraint l r t = constrained "Prim.Row.Lacks" [ l, r ] t

      -- | _Badge
      -- |   :: forall props
      -- |   . MergeProps (BadgePropsRow ()) Props_div props
      -- |   ⇒ React.Basic.ReactComponent { | props }
      -- | _Badge = unsafeCoerce _UnsafeBadge
      componentIdent = Ident ("_" <> componentName)
      componentVarExpr = Expr.ident (Sugar.local componentIdent)
      inherits' = fromMaybe (Type.constructor "React.Basic.DOM.Props_div") inherits
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

          -- | TODO: We need some sugar for this
          propsPartialArgIdent = Ident "propsPartial"
          propsPartialVar = Expr.ident (Sugar.local propsPartialIdent)

          signature = forAll { pp: "propsPartial", p: "props" } \{ p, pp } →
            let
              fun = Type.arr (Type.recordLiteral pp) jsx
            in
              mergeConstraint p (coerceConstraint pp p fun)

      -- | badgeRooted
      -- |   ∷ ∀ componentProps props propsPartial
      -- |   . MergeProps (BadgePropsRow ()) componentProps props
      -- |   ⇒ Row.Lacks "component" props
      -- |   ⇒ Coerce { | propsPartial } { | props }
      -- |   ⇒ ReactComponent { | componentProps }
      -- |   → { | propsPartial }
      -- |   → JSX
      -- | badgeRooted component propsPartial =
      -- |   element _SafeBadge props
      -- |   where
      -- |     _component :: SProxy "component"
      -- |     _component = SProxy
      -- |     props = Record.insert _component component (coerce propsPartial)
      -- |
      -- |     _SafeBadge ∷ ReactComponent { component ∷ ReactComponent { | componentProps } | props }
      -- |     _SafeBadge = unsafeCoerce _UnsafeBadge
      -- |

      test = DeclValue $ forAllValueBinding { cp: "componentProps", pp: "propsPartial", p: "props" } "badgeRooted" (SListProxy ∷ _ ("component" ::: "propsPartial" ::: SNil)) \{ bindersVars, typeVars } →
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

      -- constructorRootedDecl = declValue
      --   constructorRootedIdent
      --   [ componentArgIdent, propsPartialIdent ]
      --   (reactElemApp componentVarExpr (coerceAppExpr propsPartialVar))
      --   (Just signature)
      --   []
      --   where
      --     constructorRootedIdent = Ident (constructorName <> "Rooted")

      --     signature = forAll { pp: "propsPartial", p: "props" } \{ p, pp } →
      --       let
      --         fun = Type.arr (Type.recordLiteral pp) jsx
      --       in
      --         mergeConstraint p (coerceConstraint pp p fun)

      --     componentArgIdent = Ident "component"
      --     componentArgVar = Expr.ident (Sugar.local componentArgIdent)

      --     safeComponentIdent = Ident ("_" <> "Safe" <> componentName)
      --     safeComponentDecl =
      --       let
      --         signature = forAll { c: "componentProps" } \{ c } →
      --           reactComponentApply (recordLiteral (Type.app propsRowConstructor [ c ]))
      --       in
      --         expr

      -- -- |     _SafeBadge ∷ ReactComponent { component ∷ ReactComponent { | componentProps } | props }
      -- -- |     _SafeBadge = unsafeCoerce _UnsafeBadge



    tell (List.singleton test)
    tell (List.singleton unsafeComponentDecl.declaration)
    tell (List.singleton componentDecl.declaration)
    tell (List.singleton constructorDecl.declaration)



--         -- | For example:
--         -- | appBar :: ∀  missing option
--         -- |   .  Union optional missing (AppBarPropsOptions (PaperProps Props_div) )
--         -- |   => Record (AppBarPropsOptionsRequired optional)
--         -- |   -> JSX
--         -- | appBar = element _AppBar
--         componentConstructor =
--           let
--             signature =
--               forAllWith baseExtraVars { o: "optional", m: "missing" }
--                 $ \{ o, m } ->
--                     let
--                       fun :: AST.Type
--                       fun = Type.arr (recordApply g) Model.jsx
--                         where
--                           g :: AST.Type
--                           g = maybe o (\x -> Type.app x [ o ]) propsConstructor
-- 
--                       baseExtraVars' :: Array AST.Type
--                       baseExtraVars' = map Type.var baseExtraVars
-- 
--                       u :: AST.Type
--                       u = Type.app propsConstructor (Array.cons inherits' baseExtraVars')
--                     in
--                       constrained "Prim.Row.Union" [ o, m, u ] fun
--           in
--             declValue
--               (Ident componentName')
--               []
--               (Expr.app (Expr.ident "React.Basic.element") componentValue.var)
--               (Just signature)
-- 
--         -- | For example:
--         -- | appBar_component :: ∀  componentProps given required
--         -- |   .  Union given required (AppBarPropsOptions componentProps)
--         -- |   => Record given
--         -- |   -> JSX
--         -- | appBar_component = element _AppBar
--         componentConstructor' =
--           let
--             signature =
--               forAllWith baseExtraVars { c: "componentProps", g: "given", r: "required" }
--                 $ \{ c, g, r } ->
--                     let
--                       fun = Type.arr (recordApply g) Model.jsx
-- 
--                       baseExtraVars' = map Type.var baseExtraVars
-- 
--                       u = Type.app optionalPropsConstructor (Array.cons c optionalBaseExtraVars')
-- 
--                       r' = maybe r (\x -> Type.app x [ r ]) requiredPropsConstructor
--                     in
--                       constrained "Prim.Row.Union" [ g, r', u ] fun
--           in
--             declValue
--               (Ident $ componentName' <> "_component")
--               []
--               (Expr.app (Expr.ident "React.Basic.element") componentValue.var)
--               (Just signature)
--       tell $ List.singleton componentValue.declaration
--       tell $ List.singleton componentConstructor.declaration
--       tell $ List.singleton componentConstructor'.declaration
--       when hasStyles
--         $ do
--             let
--               -- | For example:
--               -- | appBarWithStyles :: ∀ jss jss_ required given
--               -- |   . Union given required (AppBarPropsOptions (PaperProps Props_div))
--               -- |   => Union jss jss_ AppBarPropsOptionsJSS
--               -- |   => (Theme -> Record jss)
--               -- |   -> Record given
--               -- |   -> JSX
--               -- | appBarWithStyles style = element (unsafeCoerce withStyles style _AppBar)
--               componentConstructorWithStyles =
--                 let
--                   signature =
--                     forAllWith optionalBaseExtraVars { g: "given", jss: "jss", jss_: "jss_", r: "required" }
--                       $ \{ g, jss, jss_, r } ->
--                           let
--                             style = Type.arr (Type.constructor "MUI.Core.Styles.Types.Theme") (recordApply jss)
-- 
--                             fun = Type.arr style (Type.arr (recordApply g) Model.jsx)
-- 
--                             optionalBaseExtraVars' = map Type.var optionalBaseExtraVars
-- 
--                             u = Type.app optionalPropsConstructor (Array.cons optionalPropsInherits' optionalBaseExtraVars')
--                           in
--                             constrained "Prim.Row.Union" [ g, r, u ]
--                               $ constrained "Prim.Row.Union" [ jss, jss_, Type.constructor $ componentName <> "ClassKeyOptionsJSS" ]
--                               $ fun
--                 in
--                   declValue
--                     (Ident $ componentName' <> "WithStyles")
--                     [ Ident "style" ]
--                     ( Expr.app
--                         (Expr.ident "React.Basic.element")
--                         ( Expr.app
--                             ( Expr.app
--                                 ( Expr.app
--                                     exprUnsafeCoerce
--                                     (Expr.ident "MUI.Core.Styles.WithStyles.withStyles")
--                                 )
--                                 (Expr.ident "style")
--                             )
--                             componentValue.var
--                         )
--                     )
--                     (Just signature)
--             tell $ pure componentConstructorWithStyles.declaration
-- 
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
