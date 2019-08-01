module Codegen.Model where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object (Object)

type Component =
  { name :: String
  , moduleName :: Module
  , props :: Object PropType
  , componentTypeVariable :: Maybe String
  , additionalTypeVariables :: Array String
  , classKey :: Array String
  , inherits :: PropType
  , variants :: Array Variant
  , extraCode :: Maybe String
  }

data Module
  = Path String Module
  | Name String

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
  | ReactComponent PropType
  | PropList PropType PropType
  | ParensList PropType PropType
  | TypeVariable String
  | RecordType PropType
  | Local TypeName
  | Done

data VariantProp
  = BooleanVariant
  | StringVariant String
  | StringNameVariant String String
  | NumberVariant String Number

data Variant 
  = SimpleVariant String (Array VariantProp)
  | ModuleVariant Module String (Array VariantProp)

standardComponentTypeVariable :: Maybe String
standardComponentTypeVariable = Just componentProps

componentProps :: String
componentProps = "componentProps"

classKeyJSSName :: String -> String
classKeyJSSName name = name <> "ClassKeyJSS"

classKeyGenericName :: String -> String
classKeyGenericName name = name <> "ClassKeyGenericOptions"

classKeyName :: String -> String
classKeyName name = name <> "ClassKey"

classKeyRowName :: String -> String
classKeyRowName name = name <> "ClassKeyOptions"

classKeyRowJSSName :: String -> String
classKeyRowJSSName name = name <> "ClassKeyJSSOptions"

propsName :: String -> String
propsName name = name <> "Props"

propsRowName :: String -> String
propsRowName name = name <> "PropsOptions"

jsx :: PropType
jsx = ImportProp "React.Basic" "JSX"

arrayJSX :: PropType
arrayJSX = ArrayProp jsx

reactDom :: String -> PropType
reactDom = ImportProp "React.Basic.DOM"

divProps :: PropType
divProps = reactDom "Props_div"

effectFn2 :: PropType -> PropType
effectFn2 = PropList (ImportProp "Effect.Uncurried" "EffectFn2")

syntheticEvent :: PropType
syntheticEvent = ImportProp "React.Basic.Events" "SyntheticEvent"

eventHandler :: PropType
eventHandler = ImportProp "React.Basic.Events" "EventHandler"

