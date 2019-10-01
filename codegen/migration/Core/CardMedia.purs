module Codegen.Core.CardMedia where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, classKeyName, componentProps, divProps)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "CardMedia"

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
  (Object.insert "component" $ ReactComponent $ TypeVariable componentProps) #
  (Object.insert "image" StringProp) #
  (Object.insert "src" StringProp)


classKey :: Array String
classKey = [ "root" , "media" ]

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