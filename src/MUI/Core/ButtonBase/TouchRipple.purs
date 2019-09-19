module MUI.Core.ButtonBase.TouchRipple where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_span)
import Unsafe.Coerce (unsafeCoerce)

type TouchRipplePropsOptions componentProps = 
  ( center :: Boolean
  | componentProps
  )

foreign import data TouchRippleProps :: Type

type TouchRippleClassKeyGenericOptions a =
  ( root :: a 
  , ripple :: a 
  , rippleVisible :: a 
  , ripplePulsate :: a 
  , child :: a 
  , childLeaving :: a 
  , childPulsate :: a 
  )
type TouchRippleClassKeyOptions = TouchRippleClassKeyGenericOptions String
type TouchRippleClassKeyJSSOptions = TouchRippleClassKeyGenericOptions JSS
foreign import data TouchRippleClassKey :: Type
foreign import data TouchRippleClassKeyJSS :: Type

touchRippleClassKey :: ∀  given required
  .  Union given required (TouchRippleClassKeyOptions )
  => Record given
  -> TouchRippleClassKey
touchRippleClassKey = unsafeCoerce

touchRippleClassKeyJSS :: ∀  given required
  .  Union given required (TouchRippleClassKeyJSSOptions )
  => Record given
  -> TouchRippleClassKeyJSS
touchRippleClassKeyJSS = unsafeCoerce

touchRipple :: ∀  given required
  .  Union given required (TouchRipplePropsOptions Props_span )
  => Record given
  -> JSX
touchRipple = element _TouchRipple

touchRipple_component :: ∀ componentProps given required
  .  Union given required (TouchRipplePropsOptions componentProps)
  => Record given
  -> JSX
touchRipple_component = element _TouchRipple

foreign import _TouchRipple :: ∀ a. ReactComponent a