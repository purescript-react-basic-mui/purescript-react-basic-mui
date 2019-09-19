module Codegen.Core.Button where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant(..), VariantProp(..), arrayJSX, classKeyName, componentProps, propsRowName, reactDom)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "Button"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = 
  [ SimpleVariant 
    "ColorProp"
    [ StringVariant "inherit"
    , StringVariant "primary"
    , StringVariant "secondary"
    , StringVariant "default"
    ]
  , SimpleVariant 
    "SizeProp"
    [ StringVariant "small"
    , StringVariant "medium"
    , StringVariant "large"
    ]
  , SimpleVariant 
    "VariantProp"
    [ StringVariant "text"
    , StringVariant "outlined"
    , StringVariant "contained"
    ]
  ]

inherits :: PropType
inherits = ParensList (ImportProp "MUI.Core.ButtonBase" (propsRowName "ButtonBase")) (reactDom "Props_button")

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Nothing

props :: Object PropType
props = Object.empty #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" (Local $ classKeyName name)) #
  (Object.insert "color" (Local "ColorProp")) #
  (Object.insert "component" (ReactComponent $ TypeVariable componentProps)) #
  (Object.insert "disabled" BooleanProp) #
  (Object.insert "disableFocusRipple" BooleanProp) #
  (Object.insert "disableRipple" BooleanProp) #
  (Object.insert "fullWidth" BooleanProp) #
  (Object.insert "href" StringProp) # 
  (Object.insert "size" $ Local "SizeProp") #
  (Object.insert "variant" $ Local "VariantProp")

classKey :: Array String
classKey = 
  [ "root"
  , "label"
  , "text"
  , "textPrimary"
  , "textSecondary"
  , "outlined"
  , "outlinedPrimary"
  , "outlinedSecondary"
  , "contained"
  , "containedPrimary"
  , "containedSecondary"
  , "focusVisible"
  , "disabled"
  , "colorInherit"
  , "sizeSmall"
  , "sizeLarge"
  , "fullWidth"
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