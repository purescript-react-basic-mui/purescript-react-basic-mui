module Codegen.Core.Checkbox where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant(..), VariantProp(..), classKeyName, effectFn2, jsx, propsRowName, reactDom, syntheticEvent)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "Checkbox"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants =
  [ SimpleVariant
    "ColorProp"
    [ StringVariant "primary"
    , StringVariant "secondary"
    , StringVariant "default"
    ]
  ]


inherits :: PropType
inherits = ParensList 
            (ImportProp "MUI.Core.IconButton" (propsRowName "IconButton")) 
            (ParensList 
              (ImportProp "MUI.Core.ButtonBase" (propsRowName "ButtonBase")) 
              (reactDom "Props_button")
            )

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = [ "value" ]

extraCode :: Maybe String
extraCode = Nothing

props :: Object PropType
props = Object.empty #
  (Object.insert "checked" BooleanProp) #
  (Object.insert "checkedIcon" jsx) #
  (Object.insert "classes" $ Local $ classKeyName name) #
  (Object.insert "color" $ Local "ColorProp") #
  (Object.insert "disabled" BooleanProp) #
  (Object.insert "disableRipple" BooleanProp) #
  (Object.insert "icon" jsx) #
  (Object.insert "id" StringProp) #
  (Object.insert "indeterminate" BooleanProp) #
  (Object.insert "indeterminateIcon" jsx) #
  (Object.insert "inputProps" $ RecordType (reactDom "Props_input")) #
  (Object.insert "inputRef" $ ImportProp "Foreign" "Foreign") #
  (Object.insert "onChange" $ effectFn2 $ PropList syntheticEvent $ PropList BooleanProp UnitProp) #
  (Object.insert "type" StringProp) # 
  (Object.insert "value" $ TypeVariable "value")


classKey :: Array String
classKey =
  [ "root"
  , "checked"
  , "disabled"
  , "indeterminate"
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