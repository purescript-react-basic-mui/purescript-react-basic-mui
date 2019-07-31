module MUI.Core.Grid where

import Prelude

import Codegen.Model (Module(..), PropType(..), Variant(..), VariantProp(..))
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