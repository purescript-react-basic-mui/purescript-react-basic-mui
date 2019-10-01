module Codegen.Core.CardActions where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, arrayJSX, classKeyName, divProps)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "CardActions"

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
  (Object.insert "children" arrayJSX) #
  (Object.insert "disableSpacing" BooleanProp)

classKey :: Array String
classKey = [ "root", "spacing" ]

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