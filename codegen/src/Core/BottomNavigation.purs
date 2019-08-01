module Codegen.Core.BottomNavigation where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, arrayJSX, classKeyName, componentProps, divProps, effectFn2, syntheticEvent)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "BottomNavigation"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = []

classKey :: Array String
classKey = [ "root" ]

inherits :: PropType
inherits = divProps

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = [ "value" ]

props :: Object PropType
props = Object.empty #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" $ (Local $ classKeyName name)) #
  (Object.insert "component" (ReactComponent $ TypeVariable componentProps)) #
  (Object.insert "onChange" $ effectFn2 $ PropList syntheticEvent $ PropList (TypeVariable "value") UnitProp) #
  (Object.insert "showLabels" BooleanProp) #
  (Object.insert "value" (TypeVariable "value"))

component :: Component
component = 
  { name
  , moduleName
  , props
  , componentTypeVariable
  , additionalTypeVariables
  , classKey
  , inherits
  , variants
  }



