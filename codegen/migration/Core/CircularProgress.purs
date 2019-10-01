module Codegen.Core.CircularProgress where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant(..), VariantProp(..), classKeyName, divProps)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "CircularProgress"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants =
  [ SimpleVariant
    "ColorProp"
    [ StringVariant "primary"
    , StringVariant "secondary"
    , StringVariant "inherit"
    ]
  , SimpleVariant
    "VariantProp"
    [ StringVariant "determinate"
    , StringVariant "indeterminate"
    , StringVariant "static"
    ]
  ]

inherits :: PropType
inherits = divProps

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Nothing

props :: Object PropType
props = Object.empty #
  (Object.insert "classes" $ Local $ classKeyName name) # 
  (Object.insert "color" $ Local "ColorProp") #
  (Object.insert "disableShrink" BooleanProp) #
  (Object.insert "size" NumberProp) #
  (Object.insert "thickness" NumberProp) # 
  (Object.insert "value" NumberProp) # 
  (Object.insert "variant" $ Local "VariantProp")


classKey :: Array String
classKey =
  [ "root"
  , "static"
  , "indeterminate"
  , "colorPrimary"
  , "colorSecondary"
  , "svg"
  , "circle"
  , "circleStatic"
  , "circleIndeterminate"
  , "circleDisableShrink"
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