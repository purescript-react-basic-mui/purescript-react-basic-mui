module Codegen.Core.ExpansionPanel where

import Prelude

import Codegen.Model (Component, File(..), PropType(..), arrayJSX, divProps, effectFn2, syntheticEvent)
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
  (Object.insert "onChange" $ effectFn2 $ PropList syntheticEvent $ PropList BooleanProp UnitProp) #
  (Object.insert "TransitionComponent" $ ReactComponent "MUI.Core.Transition" "TransitionPropsPartial") #
  (Object.insert "TransitionProps" $ ImportProp "MUI.Core.Transition" "TransitionPropsPartial") 

inherits :: PropType
inherits = PropList (ImportProp "MUI.Core.Paper" "PaperProps") divProps

classKey :: Array String
classKey = [ "root", "rounded", "expanded", "disabled" ]

js :: File
js = Directory "Core" (File name)

component :: Component
component = { name, js, props, classKey, inherits }
