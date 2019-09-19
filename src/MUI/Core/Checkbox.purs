module MUI.Core.Checkbox where

import Prelude

import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import MUI.Core (JSS)
import MUI.Core.ButtonBase (ButtonBasePropsOptions)
import MUI.Core.IconButton (IconButtonPropsOptions)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button, Props_input)
import React.Basic.Events (SyntheticEvent)
import Unsafe.Coerce (unsafeCoerce)

type CheckboxPropsOptions componentProps value = 
  ( checked :: Boolean
  , checkedIcon :: JSX
  , classes :: CheckboxClassKey
  , color :: ColorProp
  , disabled :: Boolean
  , disableRipple :: Boolean
  , icon :: JSX
  , id :: String
  , indeterminate :: Boolean
  , indeterminateIcon :: JSX
  , inputProps :: { | Props_input }
  , inputRef :: Foreign
  , onChange :: EffectFn2 SyntheticEvent Boolean Unit
  , type :: String
  , value :: value
  | componentProps
  )

foreign import data CheckboxProps :: Type

foreign import data ColorProp :: Type
foreign import _eqColorProp :: ColorProp -> ColorProp -> Boolean
foreign import _ordColorProp :: ColorProp -> ColorProp -> Int
instance eqColorProp :: Eq ColorProp where eq _left _right = _eqColorProp _left _right
instance ordColorProp :: Ord ColorProp where compare _left _right = compare (_ordColorProp _left _right) (_ordColorProp _right _left)

primary :: ColorProp
primary = unsafeCoerce "primary"

secondary :: ColorProp
secondary = unsafeCoerce "secondary"

default :: ColorProp
default = unsafeCoerce "default"

type CheckboxClassKeyGenericOptions a =
  ( root :: a 
  , checked :: a 
  , disabled :: a 
  , indeterminate :: a 
  , colorPrimary :: a 
  , colorSecondary :: a 
  )
type CheckboxClassKeyOptions = CheckboxClassKeyGenericOptions String
type CheckboxClassKeyJSSOptions = CheckboxClassKeyGenericOptions JSS
foreign import data CheckboxClassKey :: Type
foreign import data CheckboxClassKeyJSS :: Type

checkboxClassKey :: ∀  given required
  .  Union given required (CheckboxClassKeyOptions )
  => Record given
  -> CheckboxClassKey
checkboxClassKey = unsafeCoerce

checkboxClassKeyJSS :: ∀  given required
  .  Union given required (CheckboxClassKeyJSSOptions )
  => Record given
  -> CheckboxClassKeyJSS
checkboxClassKeyJSS = unsafeCoerce

checkbox :: ∀ value given required
  .  Union given required (CheckboxPropsOptions (IconButtonPropsOptions (ButtonBasePropsOptions Props_button)) value)
  => Record given
  -> JSX
checkbox = element _Checkbox

checkbox_component :: ∀ componentProps value given required
  .  Union given required (CheckboxPropsOptions componentProps value)
  => Record given
  -> JSX
checkbox_component = element _Checkbox

foreign import _Checkbox :: ∀ a. ReactComponent a