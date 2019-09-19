module Codegen.Core.ButtonBase.TouchRipple where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant, reactDom)
import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import Foreign.Object as Object


name :: String
name = "TouchRipple"

moduleName :: Module
moduleName = Path "MUI" (Path "Core" (Path "ButtonBase" (Name name)))

variants :: Array Variant
variants = []

inherits :: PropType
inherits = reactDom "Props_span"

componentTypeVariable :: Maybe String
componentTypeVariable = Just "componentProps"

additionalTypeVariables :: Array String
additionalTypeVariables = []

extraCode :: Maybe String
extraCode = Nothing

props :: Object PropType
props = Object.empty #
  (Object.insert "center" BooleanProp)

classKey :: Array String
classKey = 
  [ "root"
  , "ripple"
  , "rippleVisible"
  , "ripplePulsate"
  , "child"
  , "childLeaving"
  , "childPulsate"
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