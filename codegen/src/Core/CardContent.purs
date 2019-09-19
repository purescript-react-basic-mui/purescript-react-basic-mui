module Codegen.Core.CardContent where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, classKeyName, componentProps, divProps)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "CardContent"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = []

inherits :: PropType
inherits = divProps

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Nothing

props :: Object PropType
props = Object.empty #
  (Object.insert "classes" $ Local $ classKeyName name) #
  (Object.insert "component" $ ReactComponent $ TypeVariable componentProps)

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