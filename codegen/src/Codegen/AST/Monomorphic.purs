module Codegen.AST.Monomorphic where

import Prelude

import Codegen.AST (ExprF(..), RowF(..), Type, TypeF(..))
import Codegen.AST.Printers (PrintingContext(..)) as Printers
import Codegen.AST.Printers (printType)
import Control.Monad.Error.Class (throwError)
import Data.Array (uncons) as Array
import Data.Either (Either)
import Data.Foldable (all)
import Data.Functor.Mu (roll, unroll)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Matryoshka (cata)

-- | Try to build a monomorphic type for a give expression.
build :: ExprF (TypeF Type) -> Either String (TypeF Type)
build = case _ of
  ExprBoolean _ -> pure $ TypeBoolean
  ExprApp x y -> case x of
    (TypeArr arg res)
      | roll y == arg -> pure $ unroll res
    otherwise ->
      throwError
        $ "Unable to unify types in application: "
        <> "("
        <> cataM printType (roll x) Printers.StandAlone
        <> ") "
        <> "("
        <> cata printType (roll y) Printers.StandAlone
        <> ")"
  ExprArray types -> case Array.uncons types of
    Nothing -> throwError "Polymorphic array expression []"
    Just { head, tail } ->
      if all (eq (roll head) <<< roll) tail then
        pure head
      else
        throwError
          $ "Fail to unify array expressions: "
          <> joinWith ", " (map (\t -> cata printType (roll t) Printers.StandAlone) types)
  ExprIdent x ->
    throwError
      $ "Unable to handle polymorphic variable"
  ExprNumber n -> pure $ TypeNumber
  ExprRecord props -> pure $ TypeRecord (Row { labels: map roll props, tail: Nothing })
  ExprString s -> pure $ TypeString
