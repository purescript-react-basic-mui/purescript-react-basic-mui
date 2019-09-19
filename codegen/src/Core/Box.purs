module Codegen.Core.Box where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, componentProps, reactDom)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "Box"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = []

inherits :: PropType
inherits = reactDom "Props_div"

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Nothing

props :: Object PropType
props = Object.empty #
  (Object.insert "component" $ ReactComponent $ TypeVariable $ componentProps) #
  (Object.insert "clone" BooleanProp) #
  (Object.insert "css" (ImportProp "MUI.Core" "JSS"))

classKey :: Array String
classKey = [ "root" ]

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