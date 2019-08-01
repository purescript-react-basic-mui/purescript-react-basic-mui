module Codegen.Core.Badge where

import Prelude

import Codegen.Model (Module(..), PropType(..), Variant(..), VariantProp(..), Component, arrayJSX, classKeyName, componentProps, divProps, jsx)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

name :: String
name = "Badge"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = 
  [ SimpleVariant
    "ColorProp"
    [ StringVariant "default"
    , StringVariant "primary"
    , StringVariant "secondary"
    , StringVariant "error"
    ]
  , SimpleVariant 
    "VariantProp"
    [ StringVariant "standard"
    , StringVariant "dot"
    ]
  ]

inherits :: PropType
inherits = divProps

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

props :: Object PropType
props = Object.empty #
  (Object.insert "badgeContent" jsx) #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" (Local $ classKeyName name)) #
  (Object.insert "color" (Local "ColorProp")) #
  (Object.insert "component" (ReactComponent $ TypeVariable componentProps)) #
  (Object.insert "invisible" BooleanProp) #
  (Object.insert "max" NumberProp) #
  (Object.insert "showZero" BooleanProp) #
  (Object.insert "variant" (Local "VariantProp"))

classKey :: Array String
classKey =
  [ "root"
  , "badge"
  , "colorPrimary"
  , "colorSecondary"
  , "colorError"
  , "invisible"
  , "dot"
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
  }

