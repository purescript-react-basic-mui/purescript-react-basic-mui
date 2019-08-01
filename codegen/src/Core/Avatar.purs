module Codegen.Core.Avatar where

import Prelude

import Codegen.Model (Module(..), PropType(..), Variant, Component, arrayJSX, classKeyName, componentProps, divProps, reactDom)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object

name :: String
name = "Avatar"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Name name))

variants :: Array Variant
variants = []

extraCode :: Maybe String
extraCode = Nothing

props :: Object PropType
props = Object.empty #
  (Object.insert "alt" StringProp) #
  (Object.insert "children" arrayJSX) #
  (Object.insert "classes" (Local $ classKeyName name)) #
  (Object.insert "component" (ReactComponent $ TypeVariable componentProps)) #
  (Object.insert "imgProps" (RecordType $ reactDom "Props_img")) #
  (Object.insert "sizes" StringProp) #
  (Object.insert "src" StringProp) #
  (Object.insert "srcSet" StringProp)

inherits :: PropType
inherits = divProps

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

classKey :: Array String
classKey = 
  [ "root"
  , "colorDefault"
  , "img"
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

