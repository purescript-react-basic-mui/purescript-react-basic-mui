module Codegen.Core.AppBar where

import Prelude

import Codegen.Model (Module(..), PropType(..), Variant(..), VariantProp(..), Component, arrayJSX, classKeyName, divProps)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

name :: String
name = "AppBar"

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
    "PositionProp"
    [ StringVariant "fixed"
    , StringVariant "absolute"
    , StringVariant "sticky"
    , StringVariant "static"
    , StringVariant "relative"
    ]
  ]

props :: Object PropType
props = Object.empty #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" (Local $ classKeyName name)) #
  (Object.insert "color" (Local "ColorProp")) #
  (Object.insert "position" (Local "PositionProp"))

inherits :: PropType
inherits = ParensList (ImportProp "MUI.Core.Paper" "PaperProps") divProps

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Nothing

classKey :: Array String
classKey = 
  [ "root"
  , "positionFixed"
  , "positionAbsolute"
  , "positionSticky"
  , "positionStatic"
  , "positionRelative"
  , "colorDefault"
  , "colorPrimary"
  , "colorSecondary"
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

