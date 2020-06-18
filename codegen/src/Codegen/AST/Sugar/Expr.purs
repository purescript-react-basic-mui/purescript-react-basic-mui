module Codegen.AST.Sugar.Expr where

import Prelude

import Codegen.AST (Ident)
import Codegen.AST.Sugar (qualifiedIdent) as Sugar
import Codegen.AST.Types (Expr, ExprF(..), RowLabel, QualifiedName)
import Data.Functor.Mu (roll)
import Data.Map (Map)

app :: Expr -> Expr -> Expr
app f a = roll $ ExprApp f a

array :: Array Expr -> Expr
array = roll <<< ExprArray

boolean :: Boolean -> Expr
boolean = roll <<< ExprBoolean

ident :: QualifiedName Ident -> Expr
ident = roll <<< ExprIdent

ident' :: String -> Expr
ident' = ident <<< Sugar.qualifiedIdent

number :: Number -> Expr
number = roll <<< ExprNumber

record :: Map RowLabel Expr -> Expr
record = roll <<< ExprRecord

string :: String -> Expr
string = roll <<< ExprString
