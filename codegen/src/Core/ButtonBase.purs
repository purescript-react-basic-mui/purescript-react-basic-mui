module Codegen.Core.ButtonBase where

import Prelude

import Codegen.Model (Module(..), PropType(..), Variant(..), VariantProp(..), Component, arrayJSX, classKeyName, componentProps, eventHandler, propsName, reactDom)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "ButtonBase"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = 
  [ SimpleVariant
    "TypeProp"
    [ StringVariant "submit"
    , StringVariant "reset"
    , StringVariant "button"
    ]
  ]

inherits :: PropType
inherits = reactDom "Props_button"

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Just """type ButtonBaseActions = Foreign
type ButtonBaseTypeProp = Foreign
"""

props :: Object PropType
props = Object.empty #
  (Object.insert "action" (ImportProp "Foreign" "Foreign")) #
  (Object.insert "buttonRef" (ImportProp "Foreign" "Foreign")) #
  (Object.insert "centerRipple" BooleanProp) #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" (Local $ classKeyName name)) #
  (Object.insert "component" (ReactComponent (TypeVariable componentProps))) #
  (Object.insert "disabled" BooleanProp) #
  (Object.insert "disableRipple" BooleanProp) #
  (Object.insert "disableTouchRipple" BooleanProp) #
  (Object.insert "focusRipple" BooleanProp) #
  (Object.insert "focusVisibleClassName" StringProp) #
  (Object.insert "onFocusVisible" eventHandler) #
  (Object.insert "TouchRippleProps" (ImportProp "MUI.Core.ButtonBase.TouchRipple" $ propsName $ "TouchRipple")) #
  (Object.insert "type" (Local "TypeProp"))

classKey :: Array String
classKey = [ "root", "disabled", "focusVisible" ]

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