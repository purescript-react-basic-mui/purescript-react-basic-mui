module Codegen.Core.IconButton where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant(..), VariantProp(..), arrayJSX, classKeyName, propsRowName, reactDom)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "IconButton"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants =
  [ SimpleVariant
    "ColorProp"
    [ StringVariant "primary"
    , StringVariant "secondary"
    , StringVariant "default"
    , StringVariant "inherit"
    ]
  , SimpleVariant
    "EdgeProp"
    [ StringVariant "start"
    , StringVariant "end"
    , BooleanVariant
    ]
  , SimpleVariant
    "SizeProp"
    [ StringVariant "small"
    , StringVariant "medium"
    ]
  ]


inherits :: PropType
inherits = ParensList 
  (ImportProp "MUI.Core.ButtonBase" (propsRowName "ButtonBase")) 
  (reactDom "Props_button")

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Just $ "type IconButtonPropsPartial = {}"

props :: Object PropType
props = Object.empty #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" $ Local $ classKeyName name) #
  (Object.insert "color" $ Local "ColorProp") #
  (Object.insert "disabled" BooleanProp) #
  (Object.insert "disableFocusRipple" BooleanProp) #
  (Object.insert "disableRipple" BooleanProp) #
  (Object.insert "edge" $ Local "EdgeProp") #
  (Object.insert "size" $ Local "SizeProp" )

classKey :: Array String
classKey =
  [ "root"
  , "edgeStart"
  , "edgeEnd"
  , "colorInherit"
  , "colorPrimary"
  , "colorSecondary"
  , "disabled"
  , "sizeSmall"
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