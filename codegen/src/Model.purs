module Codegen.Model where

import Prelude

import Foreign.Object (Object)

data File
  = File String
  | Directory String File

type ModuleName = String
type TypeName = String
type RowTypeTypeName = String
type RowTypeModuleName = String

data PropType
  = StringProp
  | BooleanProp
  | NumberProp
  | UnitProp
  | ArrayProp PropType
  | ImportProp ModuleName TypeName
  | ReactComponent RowTypeModuleName RowTypeTypeName
  | PropList PropType PropType
  | Done

arrayJSX :: PropType
arrayJSX = ArrayProp $ ImportProp "React.Basic" "JSX"

divProps :: PropType
divProps = Done

effectFn2 :: PropType -> PropType
effectFn2 = PropList (ImportProp "Effect.Uncurried" "EffectFn2")

syntheticEvent :: PropType
syntheticEvent = ImportProp "React.Basic.Events" "SyntheticEvent"

type Component =
  { name :: String
  , js :: File
  , props :: Object PropType
  , classKey :: Array String
  , inherits :: PropType
  }