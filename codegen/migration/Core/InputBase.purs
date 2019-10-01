module Codegen.Core.InputBase where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant(..), VariantProp(..), classKeyName, divProps, eventHandler, jsx, reactDom, standardComponentTypeVariable)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

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

name :: String
name = "InputBase"

moduleName :: Module 
moduleName = Path "MUI" (Path "Core" (Name name))

componentTypeVariable :: Maybe String
componentTypeVariable = standardComponentTypeVariable

extraCode :: Maybe String
extraCode = Nothing

additionalTypeVariables :: Array String
additionalTypeVariables = [ "value" ]

inherits :: PropType
inherits = divProps

variants :: Array Variant
variants = [ SimpleVariant "MarginProp" [ StringVariant "dense", StringVariant "none" ] ]

props :: Object PropType
props = Object.empty #
  (Object.insert "autoComplete" StringProp) #
  (Object.insert "autoFocus" BooleanProp) #
  (Object.insert "classes" (Local $ classKeyName name)) #
  (Object.insert "className" StringProp) #
  (Object.insert "defaultValue" (TypeVariable "value")) #
  (Object.insert "disabled" BooleanProp) #
  (Object.insert "endAdornment" jsx) #
  (Object.insert "error" BooleanProp) #
  (Object.insert "fullWidth" BooleanProp) #
  (Object.insert "id" StringProp) #
  (Object.insert "inputComponent" (ReactComponent $ reactDom "Props_input")) #
  (Object.insert "inputProps" (RecordType $ reactDom "Props_input")) #
  (Object.insert "inputRef" (ImportProp "Foreign" "Foreign")) #
  (Object.insert "margin" (Local "MarginProp")) #
  (Object.insert "multiline" BooleanProp) #
  (Object.insert "name" StringProp) #
  (Object.insert "onChange" eventHandler ) #
  (Object.insert "placeholder" StringProp) #
  (Object.insert "readOnly" BooleanProp) #
  (Object.insert "required" BooleanProp) #
  (Object.insert "rows" NumberProp) #
  (Object.insert "rowsMax" NumberProp) #
  (Object.insert "select" BooleanProp) #
  (Object.insert "startAdornment" jsx) #
  (Object.insert "type" StringProp) #
  (Object.insert "value" (TypeVariable "value")) 

classKey :: Array String
classKey =
  [ "root"
  , "formControl"
  , "focused"
  , "disabled"
  , "adornedStart"
  , "adornedEnd"
  , "error"
  , "marginDense"
  , "multiline"
  , "fullWidth"
  , "input"
  , "inputMarginDense"
  , "inputSelect"
  , "inputMultiline"
  , "inputTypeSearch"
  , "inputAdornedStart"
  , "inputAdornedEnd"
  , "inputHiddenLabel"
  ]