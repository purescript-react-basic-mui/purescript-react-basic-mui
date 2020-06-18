module Codegen.AST.Sugar.Expr where

import Prelude

import Codegen.AST (Ident)
import Codegen.AST.Sugar (qualifiedIdent) as Sugar
import Codegen.AST.Types (Expr, ExprF(..), QualifiedName, RowLabel, TypeName(..))
import Data.Functor.Mu (roll)
import Data.Map (Map)
import Data.Maybe (Maybe(..))

app :: Expr -> Expr -> Expr
app f a = roll $ ExprApp f a

array :: Array Expr -> Expr
array = roll <<< ExprArray

boolean :: Boolean -> Expr
boolean = roll <<< ExprBoolean

ident :: QualifiedName Ident -> Expr
ident = roll <<< ExprIdent <<< { typeName: Nothing, qualifiedName: _ }

ident' :: String -> Expr
ident' = ident <<< Sugar.qualifiedIdent

identTyped :: TypeName -> QualifiedName Ident -> Expr
identTyped typeName = roll <<< ExprIdent <<< { typeName: Just typeName, qualifiedName: _ }

identTyped' :: String -> String -> Expr
identTyped' typeName = identTyped (TypeName typeName) <<< Sugar.qualifiedIdent

number :: Number -> Expr
number = roll <<< ExprNumber

record :: Map RowLabel Expr -> Expr
record = roll <<< ExprRecord

string :: String -> Expr
string = roll <<< ExprString
