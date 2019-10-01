module Codegen.AST.Sugar.Expr where

import Prelude

import Codegen.AST.Sugar.Type (name')
import Codegen.AST.Types (Expr, ExprF(..), RowLabel)
import Data.Functor.Mu (roll)
import Data.Map (Map)

app :: Expr -> Expr -> Expr
app f a = roll $ ExprApp f a

array :: Array Expr -> Expr
array = roll <<< ExprArray

boolean :: Boolean -> Expr
boolean = roll <<< ExprBoolean

ident :: String -> Expr
ident = roll <<< ExprIdent <<< name'

number :: Number -> Expr
number = roll <<< ExprNumber

record :: Map RowLabel Expr -> Expr
record = roll <<< ExprRecord

string :: String -> Expr
string = roll <<< ExprString

