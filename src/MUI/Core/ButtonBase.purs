module MUI.Core.ButtonBase where

import Prelude

import Foreign (Foreign)
import MUI.Core (JSS)
import MUI.Core.ButtonBase.TouchRipple (TouchRippleProps)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_button)
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

type ButtonBasePropsOptions componentProps = 
  ( action :: Foreign
  , buttonRef :: Foreign
  , centerRipple :: Boolean
  , children :: (Array JSX)
  , classes :: ButtonBaseClassKey
  , component :: ReactComponent { | componentProps }
  , disabled :: Boolean
  , disableRipple :: Boolean
  , disableTouchRipple :: Boolean
  , focusRipple :: Boolean
  , focusVisibleClassName :: String
  , onFocusVisible :: EventHandler
  , "TouchRippleProps" :: TouchRippleProps
  , type :: TypeProp
  | componentProps
  )

foreign import data ButtonBaseProps :: Type

type ButtonBaseActions = Foreign
type ButtonBaseTypeProp = Foreign


foreign import data TypeProp :: Type
foreign import _eqTypeProp :: TypeProp -> TypeProp -> Boolean
foreign import _ordTypeProp :: TypeProp -> TypeProp -> Int
instance eqTypeProp :: Eq TypeProp where eq _left _right = _eqTypeProp _left _right
instance ordTypeProp :: Ord TypeProp where compare _left _right = compare (_ordTypeProp _left _right) (_ordTypeProp _right _left)

submit :: TypeProp
submit = unsafeCoerce "submit"

reset :: TypeProp
reset = unsafeCoerce "reset"

button :: TypeProp
button = unsafeCoerce "button"

type ButtonBaseClassKeyGenericOptions a =
  ( root :: a 
  , disabled :: a 
  , focusVisible :: a 
  )
type ButtonBaseClassKeyOptions = ButtonBaseClassKeyGenericOptions String
type ButtonBaseClassKeyJSSOptions = ButtonBaseClassKeyGenericOptions JSS
foreign import data ButtonBaseClassKey :: Type
foreign import data ButtonBaseClassKeyJSS :: Type

buttonBaseClassKey :: ∀  given required
  .  Union given required (ButtonBaseClassKeyOptions )
  => Record given
  -> ButtonBaseClassKey
buttonBaseClassKey = unsafeCoerce

buttonBaseClassKeyJSS :: ∀  given required
  .  Union given required (ButtonBaseClassKeyJSSOptions )
  => Record given
  -> ButtonBaseClassKeyJSS
buttonBaseClassKeyJSS = unsafeCoerce

buttonBase :: ∀  given required
  .  Union given required (ButtonBasePropsOptions Props_button )
  => Record given
  -> JSX
buttonBase = element _ButtonBase

buttonBase_component :: ∀ componentProps given required
  .  Union given required (ButtonBasePropsOptions componentProps)
  => Record given
  -> JSX
buttonBase_component = element _ButtonBase

foreign import _ButtonBase :: ∀ a. ReactComponent a