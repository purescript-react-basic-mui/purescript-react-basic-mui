module MUI.Core.Box where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type BoxPropsOptions componentProps = 
  ( clone :: Boolean
  , component :: ReactComponent { | componentProps }
  , css :: JSS
  | componentProps
  )

foreign import data BoxProps :: Type

type BoxClassKeyGenericOptions a =
  ( root :: a 
  )
type BoxClassKeyOptions = BoxClassKeyGenericOptions String
type BoxClassKeyJSSOptions = BoxClassKeyGenericOptions JSS
foreign import data BoxClassKey :: Type
foreign import data BoxClassKeyJSS :: Type

boxClassKey :: ∀  given required
  .  Union given required (BoxClassKeyOptions )
  => Record given
  -> BoxClassKey
boxClassKey = unsafeCoerce

boxClassKeyJSS :: ∀  given required
  .  Union given required (BoxClassKeyJSSOptions )
  => Record given
  -> BoxClassKeyJSS
boxClassKeyJSS = unsafeCoerce

box :: ∀  given required
  .  Union given required (BoxPropsOptions Props_div )
  => Record given
  -> JSX
box = element _Box

box_component :: ∀ componentProps given required
  .  Union given required (BoxPropsOptions componentProps)
  => Record given
  -> JSX
box_component = element _Box

foreign import _Box :: ∀ a. ReactComponent a