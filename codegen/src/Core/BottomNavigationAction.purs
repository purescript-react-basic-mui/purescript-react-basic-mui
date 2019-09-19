module Codegen.Core.BottomNavigationAction where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, classKeyName, jsx, propsRowName, reactDom)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "BottomNavigationAction"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = []

inherits :: PropType
inherits = ParensList (ImportProp "MUI.Core.ButtonBase" (propsRowName "ButtonBase")) (reactDom "Props_button")

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = [ "value" ]

extraCode :: Maybe String
extraCode = Nothing

props :: Object PropType
props = Object.empty #
  (Object.insert "classes" (Local $ classKeyName name)) #
  (Object.insert "icon" jsx) #
  (Object.insert "label" jsx) #
  (Object.insert "showLabel" BooleanProp) #
  (Object.insert "value" (TypeVariable "value"))

classKey :: Array String
classKey =
  [ "root"
  , "selected"
  , "iconOnly"
  , "wrapper"
  , "label"
  ]

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
  , extraCode
  }