module Codegen.Core.ExpansionPanel where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, arrayJSX, divProps, effectFn2, syntheticEvent)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

name :: String
name = "ExpansionPanel"

props :: Object PropType
props = Object.empty #
  (Object.insert "children" arrayJSX) # 
  (Object.insert "defaultExpanded" BooleanProp) # 
  (Object.insert "disabled" BooleanProp) #
  (Object.insert "expanded" BooleanProp) #
  (Object.insert "onChange" $ effectFn2 $ PropList syntheticEvent $ PropList BooleanProp UnitProp) 
--  (Object.insert "TransitionComponent" $ ReactComponent (ImportProp "MUI.Core.Transition" (propsRowName "Transition"))) #
--  (Object.insert "TransitionProps" $ ImportProp "MUI.Core.Transition" "TransitionProps") 

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Nothing

inherits :: PropType
inherits = ParensList (ImportProp "MUI.Core.Paper" "PaperProps") divProps

classKey :: Array String
classKey = [ "root", "rounded", "expanded", "disabled" ]

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = []

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
