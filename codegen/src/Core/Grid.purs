module Codegen.Core.Grid where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant(..), VariantProp(..), arrayJSX, classKeyName, componentProps, divProps)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

name :: String
name = "Grid"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = 
  [ (ModuleVariant 
      (Path "MUI" (Path "Core" (Path "Grid" (Name "AlignContent")))) 
      "AlignContentProp"
      [ StringVariant "stretch"
      , StringVariant "center"
      , StringNameVariant "flexStart" "flex-start"
      , StringNameVariant "flexEnd" "flex-end"
      , StringNameVariant "spaceBetween" "space-between"
      , StringNameVariant "spaceAround" "space-around"
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Grid" (Name "AlignItems")))) 
      "AlignItemsProp" 
      [ StringNameVariant "flexStart" "flex-start"
      , StringVariant "center"
      , StringNameVariant "flexEnd" "flex-end"
      , StringVariant "stretch"
      , StringVariant "baseline"
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Grid" (Name "Direction")))) 
      "DirectionProp" 
      [ StringVariant "row"
      , StringNameVariant "rowReverse" "row-reverse"
      , StringVariant "column"
      , StringNameVariant "columnReverse" "column-reverse"
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Grid" (Name "Justify")))) 
      "JustifyProp" 
      [ StringNameVariant "flexStart" "flex-start"
      , StringVariant "center"
      , StringNameVariant "flexEnd" "flex-end"
      , StringNameVariant "spaceBetween" "space-between"
      , StringNameVariant "spaceAround" "space-around"
      , StringNameVariant "spaceEvenly" "space-evenly"
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Grid" (Name "GridCount")))) 
      "GridCountProp" 
      [ BooleanVariant
      , StringVariant "auto"
      , NumberVariant "one" 1.0
      , NumberVariant "two" 2.0
      , NumberVariant "three" 3.0
      , NumberVariant "four" 4.0
      , NumberVariant "five" 5.0
      , NumberVariant "six" 6.0
      , NumberVariant "seven" 7.0
      , NumberVariant "eight" 8.0
      , NumberVariant "nine" 9.0
      , NumberVariant "ten" 10.0
      , NumberVariant "eleven" 11.0
      , NumberVariant "twelve" 12.0
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Grid" (Name "Spacing")))) 
      "SpacingProp" 
      [ NumberVariant "one" 1.0
      , NumberVariant "two" 2.0
      , NumberVariant "three" 3.0
      , NumberVariant "four" 4.0
      , NumberVariant "five" 5.0
      , NumberVariant "six" 6.0
      , NumberVariant "seven" 7.0
      , NumberVariant "eight" 8.0
      , NumberVariant "nine" 9.0
      , NumberVariant "ten" 10.0
      ]
    )
  , (ModuleVariant
      (Path "MUI" (Path "Core" (Path "Grid" (Name "Wrap")))) 
      "WrapProp" 
      [ StringVariant "nowrap"
      , StringVariant "wrap"
      , StringNameVariant "wrapReverse" "wrap-reverse"
      ]
    )
  ]

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

inherits :: PropType
inherits = divProps

props :: Object PropType
props = Object.empty #
  (Object.insert "alignContent" (ImportProp "MUI.Core.Grid.AlignContent" "AlignContentProp")) #
  (Object.insert "alignItems" (ImportProp "MUI.Core.Grid.AlignItems" "AlignItemsProp")) #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" (Local $ classKeyName name)) #
  (Object.insert "component" (ReactComponent (TypeVariable componentProps))) #
  (Object.insert "container" BooleanProp) #
  (Object.insert "direction" (ImportProp "MUI.Core.Grid.Direction" "DirectionProp")) #
  (Object.insert "item" BooleanProp) #
  (Object.insert "justify" (ImportProp "MUI.Core.Grid.Justify" "JustifyProp")) #
  (Object.insert "lg" (ImportProp "MUI.Core.Grid.GridCount" "GridCountProp")) #
  (Object.insert "md" (ImportProp "MUI.Core.Grid.GridCount" "GridCountProp")) #
  (Object.insert "sm" (ImportProp "MUI.Core.Grid.GridCount" "GridCountProp")) #
  (Object.insert "spacing" (ImportProp "MUI.Core.Grid.Spacing" "SpacingProp")) #
  (Object.insert "wrap" (ImportProp "MUI.Core.Grid.Wrap" "WrapProp")) #
  (Object.insert "xl" (ImportProp "MUI.Core.Grid.GridCount" "GridCountProp")) #
  (Object.insert "xs" (ImportProp "MUI.Core.Grid.GridCount" "GridCountProp")) #
  (Object.insert "zeroMinWidth" BooleanProp)

classKey :: Array String
classKey = 
  [ "root"
  , "container"
  , "item"
  , "zeroMinWidth"
  , "direction-xs-column"
  , "direction-xs-column-reverse"
  , "direction-xs-row-reverse"
  , "wrap-xs-nowrap"
  , "wrap-xs-wrap-reverse"
  , "align-items-xs-center"
  , "align-items-xs-flex-start"
  , "align-items-xs-flex-end"
  , "align-items-xs-baseline"
  , "align-content-xs-center"
  , "align-content-xs-flex-start"
  , "align-content-xs-flex-end"
  , "align-content-xs-space-between"
  , "align-content-xs-space-around"
  , "justify-xs-center"
  , "justify-xs-flex-end"
  , "justify-xs-space-between"
  , "justify-xs-space-around"
  , "justify-xs-space-evenly"
  , "spacing-xs-1"
  , "spacing-xs-2"
  , "spacing-xs-3"
  , "spacing-xs-4"
  , "spacing-xs-5"
  , "spacing-xs-6"
  , "spacing-xs-7"
  , "spacing-xs-8"
  , "spacing-xs-9"
  , "spacing-xs-10"
  , "grid-xs-auto"
  , "grid-xs-true"
  , "grid-xs-1"
  , "grid-xs-2"
  , "grid-xs-3"
  , "grid-xs-4"
  , "grid-xs-5"
  , "grid-xs-6"
  , "grid-xs-7"
  , "grid-xs-8"
  , "grid-xs-9"
  , "grid-xs-10"
  , "grid-xs-11"
  , "grid-xs-12"
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
