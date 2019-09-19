module MUI.Core.Button where

import Prelude

import MUI.Core (JSS)
import MUI.Core.ButtonBase (ButtonBasePropsOptions)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button)
import Unsafe.Coerce (unsafeCoerce)

type ButtonPropsOptions componentProps = 
  ( children :: (Array JSX)
  , classes :: ButtonClassKey
  , color :: ColorProp
  , component :: ReactComponent { | componentProps }
  , disabled :: Boolean
  , disableFocusRipple :: Boolean
  , disableRipple :: Boolean
  , fullWidth :: Boolean
  , href :: String
  , size :: SizeProp
  , variant :: VariantProp
  | componentProps
  )

foreign import data ButtonProps :: Type

foreign import data ColorProp :: Type
foreign import _eqColorProp :: ColorProp -> ColorProp -> Boolean
foreign import _ordColorProp :: ColorProp -> ColorProp -> Int
instance eqColorProp :: Eq ColorProp where eq _left _right = _eqColorProp _left _right
instance ordColorProp :: Ord ColorProp where compare _left _right = compare (_ordColorProp _left _right) (_ordColorProp _right _left)

inherit :: ColorProp
inherit = unsafeCoerce "inherit"

primary :: ColorProp
primary = unsafeCoerce "primary"

secondary :: ColorProp
secondary = unsafeCoerce "secondary"

default :: ColorProp
default = unsafeCoerce "default"
foreign import data SizeProp :: Type
foreign import _eqSizeProp :: SizeProp -> SizeProp -> Boolean
foreign import _ordSizeProp :: SizeProp -> SizeProp -> Int
instance eqSizeProp :: Eq SizeProp where eq _left _right = _eqSizeProp _left _right
instance ordSizeProp :: Ord SizeProp where compare _left _right = compare (_ordSizeProp _left _right) (_ordSizeProp _right _left)

small :: SizeProp
small = unsafeCoerce "small"

medium :: SizeProp
medium = unsafeCoerce "medium"

large :: SizeProp
large = unsafeCoerce "large"
foreign import data VariantProp :: Type
foreign import _eqVariantProp :: VariantProp -> VariantProp -> Boolean
foreign import _ordVariantProp :: VariantProp -> VariantProp -> Int
instance eqVariantProp :: Eq VariantProp where eq _left _right = _eqVariantProp _left _right
instance ordVariantProp :: Ord VariantProp where compare _left _right = compare (_ordVariantProp _left _right) (_ordVariantProp _right _left)

text :: VariantProp
text = unsafeCoerce "text"

outlined :: VariantProp
outlined = unsafeCoerce "outlined"

contained :: VariantProp
contained = unsafeCoerce "contained"

type ButtonClassKeyGenericOptions a =
  ( root :: a 
  , label :: a 
  , text :: a 
  , textPrimary :: a 
  , textSecondary :: a 
  , outlined :: a 
  , outlinedPrimary :: a 
  , outlinedSecondary :: a 
  , contained :: a 
  , containedPrimary :: a 
  , containedSecondary :: a 
  , focusVisible :: a 
  , disabled :: a 
  , colorInherit :: a 
  , sizeSmall :: a 
  , sizeLarge :: a 
  , fullWidth :: a 
  )
type ButtonClassKeyOptions = ButtonClassKeyGenericOptions String
type ButtonClassKeyJSSOptions = ButtonClassKeyGenericOptions JSS
foreign import data ButtonClassKey :: Type
foreign import data ButtonClassKeyJSS :: Type

buttonClassKey :: ∀  given required
  .  Union given required (ButtonClassKeyOptions )
  => Record given
  -> ButtonClassKey
buttonClassKey = unsafeCoerce

buttonClassKeyJSS :: ∀  given required
  .  Union given required (ButtonClassKeyJSSOptions )
  => Record given
  -> ButtonClassKeyJSS
buttonClassKeyJSS = unsafeCoerce

button :: ∀  given required
  .  Union given required (ButtonPropsOptions (ButtonBasePropsOptions Props_button) )
  => Record given
  -> JSX
button = element _Button

button_component :: ∀ componentProps given required
  .  Union given required (ButtonPropsOptions componentProps)
  => Record given
  -> JSX
button_component = element _Button

foreign import _Button :: ∀ a. ReactComponent a