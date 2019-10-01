module Codegen.Core.CardActionArea where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, arrayJSX, classKeyName, propsRowName, reactDom)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "CardActionArea"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = []

inherits :: PropType
inherits = ParensList (ImportProp "MUI.Core.ButtonBase" (propsRowName "ButtonBase")) (reactDom "Props_button")

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Nothing

props :: Object PropType
props = Object.empty #
  (Object.insert "classes" $ Local $ classKeyName name) #
  (Object.insert "children" arrayJSX)

classKey :: Array String
classKey = [ "root", "focusVisible", "focusHighlight" ]

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