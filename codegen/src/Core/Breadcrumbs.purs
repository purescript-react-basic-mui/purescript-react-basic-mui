module Codegen.Core.Breadcrumbs where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, arrayJSX, classKeyName, componentProps, jsx, reactDom)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "Breadcrumbs"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = []

inherits :: PropType
inherits = reactDom "Props_nav"

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Nothing

props :: Object PropType
props = Object.empty #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" (Local $ classKeyName name)) #
  (Object.insert "component" (ReactComponent $ TypeVariable componentProps)) #
  (Object.insert "itemsAfterCollapse" NumberProp) #
  (Object.insert "itemsBeforeCollapse" NumberProp) #
  (Object.insert "maxItems" NumberProp) #
  (Object.insert "separator" jsx)

classKey :: Array String
classKey =
  [ "root"
  , "ol"
  , "li"
  , "separator"
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