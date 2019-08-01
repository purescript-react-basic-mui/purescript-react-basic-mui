module Codegen.Core.Typography where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant(..), VariantProp(..), arrayJSX, classKeyName, componentProps, reactDom)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

name :: String
name = "Typography"

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

inherits :: PropType
inherits = reactDom "Props_h1"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = 
  [ (ModuleVariant 
      (Path "MUI" (Path "Core" (Path "Typography" (Name "Align")))) 
      "AlignProp"
      [ StringVariant "inherit"
      , StringVariant "left"
      , StringVariant "center"
      , StringVariant "right"
      , StringVariant "justify"
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Typography" (Name "Color")))) 
      "ColorProp"
      [ StringVariant "initial"
      , StringVariant "inherit"
      , StringVariant "primary"
      , StringVariant "secondary"
      , StringVariant "textPrimary"
      , StringVariant "textSecondary"
      , StringVariant "error"
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Typography" (Name "Display")))) 
      "DisplayProp"
      [ StringVariant "initial"
      , StringVariant "block"
      , StringVariant "inline"
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Typography" (Name "Variant")))) 
      "VariantProp"
      [ StringVariant "h1"
      , StringVariant "h2"
      , StringVariant "h3"
      , StringVariant "h4"
      , StringVariant "h5"
      , StringVariant "h6"
      , StringVariant "subtitle1"
      , StringVariant "subtitle2"
      , StringVariant "body1"
      , StringVariant "body2"
      , StringVariant "caption"
      , StringVariant "button"
      , StringVariant "overline"
      , StringVariant "srOnly"
      , StringVariant "inherit"
      ]
    )
  ]


props :: Object PropType
props = Object.empty #
  (Object.insert "align" (ImportProp "MUI.Core.Typography.Align" "AlignProp")) #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" (Local (classKeyName name))) #
  (Object.insert "color" (ImportProp "MUI.Core.Typography.Color" "ColorProp")) #
  (Object.insert "component" (ReactComponent (TypeVariable componentProps ))) #
  (Object.insert "display" (ImportProp "MUI.Core.Typography.Display" "DisplayProp")) #
  (Object.insert "gutterBottom" BooleanProp) #
  (Object.insert "noWrap" BooleanProp) #
  (Object.insert "paragraph" BooleanProp) #
  (Object.insert "variant" (ImportProp "MUI.Core.Typography.Variant" "VariantProp")) #
  (Object.insert "variantMapping" (Local "VariantMapping"))

classKey :: Array String
classKey = 
  [ "root"
  , "body2"
  , "body1"
  , "caption"
  , "button"
  , "h1"
  , "h2"
  , "h3"
  , "h4"
  , "h5"
  , "h6"
  , "subtitle1"
  , "subtitle2"
  , "overline"
  , "srOnly"
  , "alignLeft"
  , "alignCenter"
  , "alignRight"
  , "alignJustify"
  , "noWrap"
  , "gutterBottom"
  , "paragraph"
  , "colorInherit"
  , "colorPrimary"
  , "colorSecondary"
  , "colorTextPrimary"
  , "colorTextSecondary"
  , "colorError"
  , "displayInline"
  , "displayBlock"
  ]

extraCode :: Maybe String
extraCode = Just """type VariantMapping =
  { h1 :: String
  , h2 :: String
  , h3 :: String
  , h4 :: String
  , h5 :: String
  , h6 :: String
  , subtitle1 :: String
  , subtitle2 :: String
  , body1 :: String
  , body2 :: String
  }
"""

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

