module Codegen.Core.Backdrop where

import Prelude

import Codegen.Model (Module(..), PropType(..), Variant, Component, classKeyName, divProps)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

name :: String
name = "Backdrop"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = []

props :: Object PropType
props = Object.empty #
  (Object.insert "classes" (Local $ classKeyName name)) #
  (Object.insert "invisible" BooleanProp) #
  (Object.insert "open" BooleanProp) #
  (Object.insert "transitionDuration" NumberProp)

classKey :: Array String
classKey = [ "root", "invisible" ]

inherits :: PropType
inherits = divProps

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

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