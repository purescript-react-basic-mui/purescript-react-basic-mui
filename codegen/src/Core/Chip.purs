module Codegen.Core.Chip where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant(..), VariantProp(..), arrayJSX, classKeyName, componentProps, divProps, eventHandler, jsx)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "Chip"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants =
  [ (ModuleVariant 
      (Path "MUI" (Path "Core" (Path "Chip" (Name "Color")))) 
      "ColorProp"
      [ StringVariant "primary"
      , StringVariant "secondary"
      , StringVariant "default"
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Chip" (Name "Size")))) 
      "SizeProp"
      [ StringVariant "small"
      , StringVariant "medium"
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Chip" (Name "Variant")))) 
        "VariantProp"
        [ StringVariant "default"
        , StringVariant "outlined"
        ]
    )
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
  (Object.insert "avatar" jsx) #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" $ Local $ classKeyName name) #
  (Object.insert "clickable" BooleanProp) #
  (Object.insert "color" $ (ImportProp "MUI.Core.Chip.Color" "ColorProp")) #
  (Object.insert "component" $ ReactComponent $ TypeVariable $ componentProps) #
  (Object.insert "deleteIcon" jsx) #
  (Object.insert "icon" jsx) #
  (Object.insert "label" jsx) #
  (Object.insert "onDelete" eventHandler) #
  (Object.insert "size" $ (ImportProp "MUI.Core.Chip.Size" "SizeProp")) #
  (Object.insert "variant" $ (ImportProp "MUI.Core.Chip.Variant" "VariantProp"))

classKey :: Array String
classKey =
  [ "root"
  , "sizeSmall"
  , "colorPrimary"
  , "colorSecondary"
  , "clickable"
  , "clickableColorPrimary"
  , "clickableColorSecondary"
  , "deletable"
  , "deletableColorPrimary"
  , "deletableColorSecondary"
  , "outlined"
  , "outlinedPrimary"
  , "outlinedSecondary"
  , "avatar"
  , "avatarSmall"
  , "avatarColorPrimary"
  , "avatarColorSecondary"
  , "avatarChildren"
  , "icon"
  , "iconSmall"
  , "iconColorPrimary"
  , "iconColorSecondary"
  , "label"
  , "labelSmall"
  , "deleteIcon"
  , "deleteIconSmall"
  , "deleteIconColorPrimary"
  , "deleteIconColorSecondary"
  , "deleteIconOutlinedColorPrimary"
  , "deleteIconOutlinedColorSecondary"
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