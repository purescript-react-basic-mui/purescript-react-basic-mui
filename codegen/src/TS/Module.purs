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
-- | `purescript-read-dts` provides an instantiation function but it is
-- | really, really simple and incomplete.
-- |
-- | This is a proposition of a less generic so a bit more
-- | restricted approach to the problem. Strategy is simple. Build a virtual
-- | module (or phisical if you prefer ;-)) with an instantiated version
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
import Codegen.AST (Expr, ExprF(..), Ident(..), RowF(..), RowLabel, Type, TypeF(..), TypeName(..), Union(..), UnionMember(..))
import Codegen.AST.Sugar.Expr (app, boolean, ident, number, string) as Expr
import Codegen.TS.Types (M)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT(..), mapExceptT, withExceptT)
import Control.Monad.State (State, modify_)
import Data.Array (catMaybes)
import Data.Array (singleton) as Array
import Data.Either (Either(..))
import Data.Functor.Mu (roll)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..)) as List
import Data.List (List)
import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Map.Internal (keys) as Map.Internal
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.Newtype (unwrap)
import Data.String (Pattern(..))
import Data.String (contains) as String
import Data.String.Extra (camelCase, pascalCase)
import Data.Traversable (for, sequence)
import Data.Tuple (Tuple(..))
import Matryoshka (AlgebraM)
import ReadDTS.AST (Application') as ReadDTS
import ReadDTS.AST (TypeConstructor(..), build) as ReadDTS.AST
import ReadDTS.Instantiation (Type, instantiate) as ReadDTS.Instantiation
import ReadDTS.Instantiation (TypeF(..)) as Instantiation

type Declaration =
  { defaultInstance :: ReadDTS.Instantiation.Type
  , typeConstructor :: ReadDTS.AST.TypeConstructor ReadDTS.Application'
  }

type LocalTypeName = String

type Declarations = Map LocalTypeName Declaration

-- | Try to grab all exported types from a given module
-- | and try to instantiate them without arguments producing
-- | so called "default instances".
declarations
  :: { path :: String , source :: Maybe String }
  -> M Declarations
declarations file = do
  typeConstructors <- ExceptT $ ReadDTS.AST.build { strictNullChecks: false } file
  let
    known = case _ of
      ReadDTS.AST.UnknownTypeConstructor _ -> Nothing
      tc@(ReadDTS.AST.Interface { name }) -> Just { name, tc }
      tc@(ReadDTS.AST.TypeAlias { name }) -> Just { name, tc }
  Map.fromFoldable <<< catMaybes <$> for typeConstructors (known >>> case _ of
    Just { name, tc } -> do
      t <- mapExceptT (unwrap >>> pure)
        $ withExceptT Array.singleton
        $ ReadDTS.Instantiation.instantiate tc []
      pure $ Just (Tuple name { defaultInstance: t, typeConstructor: tc })
    Nothing -> pure Nothing)

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

type UnionTypes = List Union

type UnionTypeName = String

type ComponentAlgebraM a = ExceptT String (State UnionTypes) a

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
union
  :: Maybe RowLabel
  -> Array PossibleType
  -> ComponentAlgebraM (Either Type Union)
union (Just l) props = case props of
  -- | Typescript (in its strict mode) handles optional field through
  -- | union with `undefined` value.  Do we really want to IGNORE this
  -- | on PS side and pretend that everything is optional?
  -- | This is dirty hack to ignore `undefined`:
  -- |
  -- | [ UnionMember UnionUndefined, ProperType t ] -> pure $ Left $ t
  -- | [ ProperType t, UnionMember UnionUndefined ] -> pure $ Left $ t
  otherwise -> do
    props' <- for props $ case _ of
      UnionMember p -> pure p
      p -> throwError $
        "Unable to build a variant from non variant props for: " <> l <> ", " <> show p
    -- | Currently building only local variants
    pure $ Right $ Union
      { name: TypeName (pascalCase l), moduleName: Nothing }
      props'
union Nothing _ = throwError
  "Unable to build anonymous Union..."


-- | `union'` constructor which adds new union to the cache
union'
  :: Maybe RowLabel
  -> Array PossibleType
  -> ComponentAlgebraM Type
union' label vps = union label vps >>= case _ of
  Left t -> pure $ t
  Right v@(Union qn@{ name, moduleName: m } _) -> do
    -- | TODO: Validate:
    -- | * check if a given variant declaration is already defined
    -- | * check if already defined variant with the same name has
    -- |  the same structure
    when (isJust m) $
      throwError "External variants not implemented yet"
    modify_ (List.Cons v)
    pure $ roll $ TypeConstructor $ qn


-- | Given a TypeScript type representation try to build an AST for it.
-- |
-- | During the processes we are accumulating a list of union types which
-- | were encountered as the subnodes. For these subnotes we set
-- | type reference for created union type and store the union in our cache.
-- |
-- | Finaly we hopefully get an AST of the type and list of related
-- | union types which should be declared too.
-- |
-- | TODO: At the moment we are not detecting collisions and provide
-- | only local module union declaration.
astAlgebra :: AlgebraM
  ComponentAlgebraM
  Instantiation.TypeF
  PossibleType
astAlgebra = case _ of
  (Instantiation.Any) -> throwError "Unable to handle Any type"
  (Instantiation.Array (PossibleUnion vs)) ->
    properType <<< TypeArray <=< union' Nothing $ vs
  (Instantiation.Array v@(UnionMember _)) ->
    properType <<< TypeArray <=< union' Nothing $ [ v ]
  (Instantiation.Array (ProperType t)) ->
    properType $ TypeArray t
  Instantiation.Boolean -> properType TypeBoolean
  (Instantiation.BooleanLiteral b) -> unionMember $ UnionBoolean b
  (Instantiation.Intersection _ _) -> throwError "Unable to handle uninstantiated intersection"
  Instantiation.Null -> unionMember $ UnionNull
  Instantiation.Number -> properType $ TypeNumber
  (Instantiation.Object _ ts) ->
    ProperType <<< roll <<< TypeRecord <<< Row <<< { tail: Nothing, labels: _ } <$> ts'
    where
      step propName { "type": PossibleUnion vs } = union' (Just propName) vs
      step propName { "type": v@(UnionMember _) } = union' (Just propName) [ v ]
      step _ { "type": ProperType t } = pure $ t
      ts' = sequence $ mapWithIndex step ts
  Instantiation.String -> properType TypeString
  (Instantiation.Tuple _) -> throwError "Tuple handling is on the way but still not present..."
  (Instantiation.NumberLiteral n) ->
    let
      constructor = fromMaybe ("_" <> show n) $ Map.lookup n numberLiteralConstructor
    in
      unionMember $ UnionNumber constructor n
  (Instantiation.StringLiteral s) -> unionMember $ if (String.contains (Pattern "-") s)
    then UnionStringName (pascalCase s) s
    else UnionString s
  Instantiation.Undefined -> unionMember $ UnionUndefined
  (Instantiation.Union ms) -> pure $ PossibleUnion ms
  (Instantiation.Unknown err) -> throwError $
    "ReadDTS was not able to instantiate given value: " <> err
  where
    properType = pure <<< ProperType <<< roll
    unionMember = pure <<< UnionMember

    numberLiteralConstructor :: Map Number UnionTypeName
    numberLiteralConstructor = Map.fromFoldable
      [ Tuple 1.0 "one"
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
exprUnsafeCoerce = Expr.ident "Unsafe.Coerce.unsafeCoerce"

exprUnsafeCoerceApp :: Expr -> Expr
exprUnsafeCoerceApp = Expr.app exprUnsafeCoerce

exprUndefined :: Expr
exprUndefined = Expr.ident "Foreign.NullOrUndefined.undefined"

exprNull ∷ Expr
exprNull = Expr.ident "Foreign.NullOrUndefined.null"

-- | Creates declarations for an union:
-- | * a foreign type declaration for given `Union`
-- | * a record value declaration which contains "constructors"
-- |  for this union type
-- |
-- | All declarations are built in local module contex.
unionDeclarations
  :: TypeName
  -> Array UnionMember
  -> { constructors :: AST.Declaration
     , type :: AST.Declaration
     }
unionDeclarations typeName@(TypeName name) members =
  { "type": AST.DeclForeignData { typeName } -- , "kind": Nothing }
  , constructors: AST.DeclValue
    { expr
    , ident: Ident (camelCase name)
    , signature: Just signature
    }
  }
  where
    member (UnionBoolean b) = Tuple (show b) $ exprUnsafeCoerceApp (Expr.boolean b)
    member (UnionString s) = Tuple s $ exprUnsafeCoerceApp (Expr.string s)
    member (UnionStringName n s) = Tuple n $ exprUnsafeCoerceApp (Expr.string s)
    member UnionNull = Tuple "null" $ exprUnsafeCoerceApp exprNull
    member (UnionNumber n v) = Tuple n $ exprUnsafeCoerceApp (Expr.number v)
    member UnionUndefined = Tuple "undefined" $ exprUnsafeCoerceApp exprUndefined

    members' = Map.fromFoldable $ map member members
    expr = roll $ ExprRecord $ members'

    type_ = roll $ TypeConstructor { name: typeName, moduleName: Nothing }
    signature
      = roll
      <<< TypeRecord
      <<< Row
      <<< { tail: Nothing, labels: _ }
      <<< Map.fromFoldable
      <<< map (flip Tuple type_ )
      <<< Map.Internal.keys
      $ members'

