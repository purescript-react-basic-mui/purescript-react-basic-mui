module MUI.Core.Grid where

import Prelude

import Codegen.Model (Module(..), PropType(..), Variant(..), VariantProp(..), Component, divProps)
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
--  (Object.insert "alignItems") #
--  (Object.insert "children") #
--  (Object.insert "classes") #
--  (Object.insert "component") #
--  (Object.insert "container") #
--  (Object.insert "direction") #
--  (Object.insert "item") #
--  (Object.insert "justify") #
--  (Object.insert "lg") #
--  (Object.insert "md") #
--  (Object.insert "sm") #
--  (Object.insert "spacing") #
--  (Object.insert "wrap") #
--  (Object.insert "xl") #
--  (Object.insert "xs") #
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
