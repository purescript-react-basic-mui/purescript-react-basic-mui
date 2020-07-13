module Codegen.TS.Module where

-- | This module provides a quite generic way of approaching conversion
-- | from TypeScript (represented as `ReadDTS.Instantiation.AST`)
-- | to PureScript types (represeted by our `Codegen.AST`).
-- |
-- | Instantiation of TypeScript interfaces can be really complicated
-- | because of the broad spectrum of TS type level constructs like maps
-- | or a zoo of the "utility types" like `Omit`, `Exclude` etc.
-- | Additionally TS compiler public API seems to be a bit limited and it is even
-- | hard to fetch full type information from it in those advanced cases.
-- | `purescript-read-dts` provides a specialization function but it is
-- | really, really simple and incomplete.
-- |
-- | This is a proposition of a less generic so a bit more
-- | restricted approach to the problem. Strategy is simple. Build a virtual
-- | module (or phisical if you prefer ;-)) with a specialized version
-- | of a given interface so TypeScript compiler do the hard work for us.
-- | Then we can process effects of its substitution.
-- |
-- | For example - instead of processing generic
-- | `@material-ui/core/Fab/Fab.d.ts` we can provide
-- | and process this value as an input module:
-- | ```
-- |  file =
-- |    { path: `MyVirtualModuleWithInstantiatedFab.d.ts`
-- |    , source:
-- |        "import { FabProps } from '@material-ui/core/Fab/Fab'\n" <>
-- |        "export type FabPropsInstance = FabProps;"
-- |    }
-- | ```
-- |
-- | In the above example we are using default type variables from the Fab
-- | interface but we can of course provide parameters to this constructor
-- | like `FabProps<MyType, OtherType>` if we want to.
-- |
-- | We do use this strategy in `Codegen.TS.MUI`.

import Prelude

import Codegen.AST (Declaration(..)) as AST
import Codegen.AST (Expr, ExprF(..), Ident(..), RowF(..), RowLabel, Type, TypeName(..), Union(..))
import Codegen.AST.Imports (ImportAlias)
import Codegen.AST.Sugar (SListProxy(..), declInstance, forAllValueBinding, valueBindingFields)
import Codegen.AST.Sugar.Expr (app, boolean, ident', identTyped', number, string) as Expr
import Codegen.AST.Sugar.Type (app, arr, constructor) as Type
import Codegen.AST.Sugar.Type (name')
import Codegen.AST.Types (TypeF(..), UnionMember(..), ValueBindingFields(..), reservedNames)
import Codegen.TS.Types (M)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT, except, mapExceptT, withExceptT)
import Control.Monad.Reader (class MonadReader, ReaderT, asks)
import Control.Monad.State (State, get, modify_, put)
import Control.Monad.State.Trans (evalStateT)
import Control.Monad.Trans.Class (lift)
import Data.Array (catMaybes)
import Data.Array (singleton) as Array
import Data.Char.Unicode (toLower) as Unicode
import Data.Either (Either(..))
import Data.Foldable (all, foldMap)
import Data.Functor.Mu (roll, unroll)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens (view)
import Data.Lens.Record (prop)
import Data.List (List)
import Data.List (singleton) as List
import Data.Map (Map)
import Data.Map (fromFoldable, insert, lookup) as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.Set (member) as Set
import Data.String (Pattern(..))
import Data.String (contains) as String
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray, uncons) as Data.String.CodeUnits
import Data.String.Extra (pascalCase)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Effect.Class (liftEffect)
import Matryoshka (AlgebraM)
import ReadDTS.AST (Application', TypeConstructor(..), build) as ReadDTS.AST
import ReadDTS.Instantiation (Type, instantiate, TypeF(..)) as ReadDTS.Instantiation
import Record.Extra (SNil)
import Type.Prelude (SProxy(..))

type Declaration
  = { defaultInstance :: ReadDTS.Instantiation.Type
    , typeConstructor :: ReadDTS.AST.TypeConstructor ReadDTS.AST.Application'
    }

type LocalTypeName
  = String

type Declarations
  = Map LocalTypeName Declaration

-- | Try to grab all exported types from a given module
-- | and try to instantiate them without arguments producing
-- | so called "default instances".
buildAndInstantiateDeclarations ::
  { path :: String, source :: Maybe String } ->
  M Declarations
buildAndInstantiateDeclarations file = do
  typeConstructors <- liftEffect (ReadDTS.AST.build { strictNullChecks: false } file) >>= except
  let
    known :: ReadDTS.AST.TypeConstructor ReadDTS.AST.Application' -> Maybe { name :: String, tc :: ReadDTS.AST.TypeConstructor ReadDTS.AST.Application' }
    known = case _ of
      ReadDTS.AST.UnknownTypeConstructor _ -> Nothing
      ReadDTS.AST.FunctionSignature _ -> Nothing
      tc@(ReadDTS.AST.Interface { name }) -> Just { name, tc }
      tc@(ReadDTS.AST.TypeAlias { name }) -> Just { name, tc }
  Map.fromFoldable <<< catMaybes
    <$> for typeConstructors
        ( known
            >>> case _ of
                Just { name, tc } -> do
                  t <-
                    mapExceptT (unwrap >>> pure)
                      $ withExceptT Array.singleton
                      $ ReadDTS.Instantiation.instantiate tc []
                  pure $ Just (Tuple name { defaultInstance: t, typeConstructor: tc })
                Nothing -> pure Nothing
        )

-- | During a fold over typescript module types we handle unions and their members
-- | using the type defined below. It allows us to aggregate members of a union during
-- | fold so they can be used "one level up" when we finally accumulate them into
-- | a single `AST.Union`.
data PossibleType
  = ProperType Type
  | UnionMember UnionMember
  | PossibleUnion (Array PossibleType)

derive instance genericPossiblePropType :: Generic PossibleType _

instance showPossiblePropType :: Show PossibleType where
  show t = genericShow t

type UnionTypes
  = Map TypeName Union

type UnionTypeName
  = String

type ComponentAlgebraM a
  = ReaderT
      { unionName ∷ String → Array UnionMember → Maybe TypeName }
      (ExceptT String (State UnionTypes))
      a

-- | Try to build an union from provided cases. For example if we
-- | have something like `8 | "a" | null` on the TypeScript side
-- | we are going to get here an array:
-- |  ```
-- |  [ UnionMember (UnionNumber 8)
-- |  , UnionMember (UnionString "a")
-- |  , UnionMember UnionNull
-- |  ]
-- | ```
-- |
-- | If a given union was encountered in the context of a record
-- | then the label of a property for which this value was defined
-- | is passed as the first argument.
union ::
  Maybe RowLabel ->
  Array PossibleType ->
  ComponentAlgebraM (Either Type Union)
union (Just l) props = do
  -- | XXX: This flow can be a bit broken when we consider
  -- |      `strictNullChecks: true` because we are going to
  -- |      get types here.
  -- |
  -- | Typescript (in its strict mode) handles optional field through
  -- | union with `undefined` value.  Do we really want to IGNORE this
  -- | on PS side and pretend that everything is optional?
  -- | This is dirty hack to ignore `undefined`:
  -- |
  -- | case props of
  -- |   [ UnionMember UnionUndefined, ProperType t ] -> pure $ Left $ t
  -- |   [ ProperType t, UnionMember UnionUndefined ] -> pure $ Left $ t
  -- |   ...
  props' <-
    flip evalStateT 0 $ for props
      $ case _ of
          UnionMember p -> pure p
          ProperType t -> do
            -- | Really naive naming convention but maybe it will
            -- | somewhat work in "most" simple scenarios
            n <- case unroll t of
              TypeNumber -> pure "number"
              TypeString -> pure "string"
              t' -> do
                idx <- get
                put (idx + 1)
                let
                  n = case t' of
                    TypeRecord _ -> "record"
                    otherwise -> l

                  n' =
                    if idx > 0 then
                      (n <> show idx)
                    else
                      n
                pure n'
            pure $ UnionConstructor n t
          p ->
            lift $ throwError
              $ "Unable to build a variant from non variant props for: "
              <> l
              <> ", "
              <> show p

  unionName ← asks _.unionName
  let
    name = fromMaybe (typeName l) (unionName l props')
  -- | Currently building only local variants
  pure $ Right
    $ Union
        { name: name, moduleName: Nothing }
        props'
  where
  -- | TOD:
  -- | * Guard against scope naming collisions too
  typeName label = TypeName $
    if label `Set.member` reservedNames then
      (pascalCase label) <> "_"
    else
      pascalCase label
-- | Escape hatch
union Nothing props = union (Just "Anonymous") props

-- union Nothing _ = throwError
--   "Unable to build anonymous Union..."
-- | `union'` constructor which adds new union to the cache
union' ::
  Maybe RowLabel ->
  Array PossibleType ->
  ComponentAlgebraM Type
union' label vps =
  union label vps
    >>= case _ of
        Left t -> pure $ t
        Right v@(Union qn@{ name, moduleName: m } _) -> do
          -- | TODO: Validate:
          -- | * check if a given variant declaration is already defined
          -- | * check if already defined variant with the same name has
          -- |  the same structure
          when (isJust m)
            $ throwError "External variants not implemented yet"
          modify_ (Map.insert name v)
          pure $ roll $ TypeConstructor $ qn

-- | Given a TypeScript type representation try to build an AST for it.
-- |
-- | During the processes we are accumulating a list of union types which
-- | were encountered as the subnodes. For these subnodes we set
-- | type reference for created union type and store the union in our cache.
-- |
-- | Finaly we hopefully get an AST of the type and list of related
-- | union types which should be declared too.
-- |
-- | TODO: At the moment we are not detecting collisions and provide
-- | only local module union declaration.
astAlgebra ::
  AlgebraM
    ComponentAlgebraM
    ReadDTS.Instantiation.TypeF
    PossibleType
astAlgebra = case _ of
  (ReadDTS.Instantiation.Any) -> throwError "Unable to handle Any type"
  (ReadDTS.Instantiation.Function _) -> throwError "Unable to handle Function type"
  (ReadDTS.Instantiation.Void) -> throwError "Unable to handle Void type"
  (ReadDTS.Instantiation.Array (PossibleUnion vs)) -> properType <<< TypeArray <=< union' Nothing $ vs
  (ReadDTS.Instantiation.Array v@(UnionMember _)) -> properType <<< TypeArray <=< union' Nothing $ [ v ]
  (ReadDTS.Instantiation.Array (ProperType t)) -> properType $ TypeArray t
  ReadDTS.Instantiation.Boolean -> properType TypeBoolean
  (ReadDTS.Instantiation.BooleanLiteral b) -> unionMember $ UnionBoolean b
  (ReadDTS.Instantiation.Intersection _ _) -> throwError "Unable to handle uninstantiated intersection"
  ReadDTS.Instantiation.Null -> unionMember $ UnionNull
  ReadDTS.Instantiation.Number -> properType $ TypeNumber
  (ReadDTS.Instantiation.Object _ ts) -> ProperType <<< roll <<< TypeRecord <<< Row <<< { tail: Nothing, labels: _ } <$> ts'
    where
    typeOpt = if _
      then roll <<< TypeOpt
      else identity
    step propName { "type": PossibleUnion vs, optional } = typeOpt optional <$> union' (Just propName) vs
    step propName { "type": v@(UnionMember _), optional } = typeOpt optional <$> union' (Just propName) [ v ]
    step _ { "type": ProperType t, optional } = pure $ typeOpt optional t

    ts' = sequence $ mapWithIndex step ts
  ReadDTS.Instantiation.String -> properType TypeString
  (ReadDTS.Instantiation.Tuple _) -> throwError "Tuple handling is on the way but still not present..."
  (ReadDTS.Instantiation.NumberLiteral n) ->
    let
      constructor = fromMaybe ("_" <> show n) $ Map.lookup n numberLiteralConstructor
    in
      unionMember $ UnionNumber constructor n
  (ReadDTS.Instantiation.StringLiteral s) ->
    unionMember
      $ if (String.contains (Pattern "-") s) then
          UnionStringName (pascalCase s) s
        else
          UnionString s
  ReadDTS.Instantiation.Undefined -> unionMember $ UnionUndefined
  (ReadDTS.Instantiation.Union ms) -> pure $ PossibleUnion ms
  (ReadDTS.Instantiation.Unknown err) ->
    throwError
      $ "ReadDTS was not able to instantiate given value: "
      <> err
  where
  properType = pure <<< ProperType <<< roll

  unionMember = pure <<< UnionMember

  numberLiteralConstructor :: Map Number UnionTypeName
  numberLiteralConstructor =
    Map.fromFoldable
      [ Tuple 0.0 "zero"
      , Tuple 1.0 "one"
      , Tuple 2.0 "two"
      , Tuple 3.0 "three"
      , Tuple 4.0 "four"
      , Tuple 5.0 "five"
      , Tuple 6.0 "six"
      , Tuple 7.0 "seven"
      , Tuple 8.0 "eight"
      , Tuple 9.0 "nine"
      , Tuple 10.0 "ten"
      , Tuple 11.0 "eleven"
      , Tuple 12.0 "twelve"
      ]

exprUnsafeCoerce :: Expr
exprUnsafeCoerce = Expr.ident' "Unsafe.Coerce.unsafeCoerce"

exprUnsafeCoerceApp :: Expr -> Expr
exprUnsafeCoerceApp = Expr.app exprUnsafeCoerce

exprUndefined :: Expr
exprUndefined = Expr.ident' "Foreign.NullOrUndefined.undefined"

exprSProxy :: String -> { value :: ValueBindingFields, var ∷ Expr }
exprSProxy label =
  let
    name = "_" <> label
  in
    { var: Expr.ident' name
    , value: forAllValueBinding (SListProxy ∷ _ SNil) name (SListProxy ∷ _ SNil) \{} ->
        { expr: Expr.identTyped' "SProxy" "Type.Prelude.SProxy"
        , signature: Just $ Type.app (Type.constructor "Type.Prelude.SProxy") [ roll $ TypeSymbol label ]
        , whereBindings: []
        }
    }


exprNull :: Expr
exprNull = Expr.ident' "Foreign.NullOrUndefined.null"

-- | Creates declarations for an union:
-- | * a foreign type declaration for given `Union`
-- | * a record value declaration which contains "constructors"
-- |  for this union type
-- | * an Eq instance for trivial cases
-- |
-- | All declarations are built in local module contex.
unionDeclarations
  :: forall m
  . MonadReader ImportAlias m
  => TypeName
  -> Array UnionMember
  -> m
    { constructors :: AST.Declaration
    , instances :: List AST.Declaration
    , type :: AST.Declaration
    }
unionDeclarations typeName@(TypeName name) members = do

  -- | We are able to provide Eq instance based on `shallowEq`
  -- | whenever there are only literal members of a given
  -- | union.
  instances <-
    if all (not <<< isConstructor) members then
      List.singleton <$>
        declInstance
          (name' "Prelude.Eq")
          [ type_ ]
          [ valueBindingFields (Ident "eq") [] (Expr.ident' "Unsafe.Reference.unsafeRefEq") Nothing [] ]
    else
      pure mempty
  pure
    { "type": AST.DeclForeignData { typeName } -- , "kind": Nothing }
    , constructors:
      AST.DeclValue $ ValueBindingFields
        { value:
          { expr
          , binders: []
          , name: Ident (downfirst name)
          , whereBindings: []
          }
        , signature: Just signature
        }
    , instances
    }
  where
  downfirst :: String -> String
  downfirst =
    Data.String.CodeUnits.uncons
      >>> foldMap \{ head, tail } ->
          Data.String.CodeUnits.singleton (Unicode.toLower head) <> tail

  toUnicodeLower :: String -> String
  toUnicodeLower = Data.String.CodeUnits.toCharArray >>> map Unicode.toLower >>> Data.String.CodeUnits.fromCharArray

  type_ = roll $ TypeConstructor { name: typeName, moduleName: Nothing }

  literalValue e = { sig: type_, expr: e }

  isConstructor (UnionConstructor _ _) = true
  isConstructor _ = false


  member (UnionBoolean b) = Tuple (show b) $ literalValue $ exprUnsafeCoerceApp (Expr.boolean b)
  member (UnionString s) = Tuple s $ literalValue $ exprUnsafeCoerceApp (Expr.string s)
  member (UnionStringName n s) = Tuple n $ literalValue $ exprUnsafeCoerceApp (Expr.string s)
  member UnionNull = Tuple "null" $ literalValue $ exprUnsafeCoerceApp exprNull
  member (UnionNumber n v) = Tuple n $ literalValue $ exprUnsafeCoerceApp (Expr.number v)
  member UnionUndefined = Tuple "undefined" $ literalValue $ exprUnsafeCoerceApp exprUndefined
  member (UnionConstructor n t) = Tuple n $ { sig: Type.arr t type_, expr: exprUnsafeCoerce }

  members' = Map.fromFoldable $ map member members

  _expr = prop (SProxy :: SProxy "expr")

  _sig = prop (SProxy :: SProxy "sig")

  _type = prop (SProxy :: SProxy "type")

  expr = roll $ ExprRecord $ map (view _expr) $ members'

  signature =
    roll
      <<< TypeRecord
      <<< Row
      <<< { tail: Nothing, labels: _ }
      -- | TODO: What should go here into optional position?
      <<< map (view _sig)
      $ members'
