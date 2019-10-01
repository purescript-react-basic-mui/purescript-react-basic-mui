module Codegen.Core.CardHeader where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, classKeyName, componentProps, divProps, jsx, propsName)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "CardHeader"

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
  (Object.insert "action" jsx) #
  (Object.insert "avatar" jsx) #
  (Object.insert "classes" $ Local $ classKeyName name) #
  (Object.insert "component" $ ReactComponent $ TypeVariable componentProps) #
  (Object.insert "disableTypography" BooleanProp) #
  (Object.insert "subheader" jsx) #
  (Object.insert "subheaderTypographyProps" (ImportProp "MUI.Core.Typography" $ propsName "Typography")) #
  (Object.insert "title" jsx) #
  (Object.insert "titleTypographyProps" (ImportProp "MUI.Core.Typography" $ propsName "Typography")) 

classKey :: Array String
classKey = 
  [ "root"
  , "avatar"
  , "action"
  , "content"
  , "title"
  , "subheader"
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