module MUI.Core.Backdrop where

import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import Unsafe.Coerce (unsafeCoerce)

type BackdropPropsOptions componentProps = 
  ( classes :: BackdropClassKey
  , invisible :: Boolean
  , open :: Boolean
  , transitionDuration :: Number
  | componentProps
  )

foreign import data BackdropProps :: Type



type BackdropClassKeyGenericOptions a =
  ( root :: a 
  , invisible :: a 
  )
type BackdropClassKeyOptions = BackdropClassKeyGenericOptions String
type BackdropClassKeyJSSOptions = BackdropClassKeyGenericOptions JSS
foreign import data BackdropClassKey :: Type
foreign import data BackdropClassKeyJSS :: Type

backdropClassKey :: ∀  given required
  .  Union given required (BackdropClassKeyOptions )
  => Record given
  -> BackdropClassKey
backdropClassKey = unsafeCoerce

backdropClassKeyJSS :: ∀  given required
  .  Union given required (BackdropClassKeyJSSOptions )
  => Record given
  -> BackdropClassKeyJSS
backdropClassKeyJSS = unsafeCoerce

backdrop :: ∀  given required
  .  Union given required (BackdropPropsOptions Props_div )
  => Record given
  -> JSX
backdrop = element _Backdrop

backdrop_component :: ∀ componentProps given required
  .  Union given required (BackdropPropsOptions componentProps)
  => Record given
  -> JSX
backdrop_component = element _Backdrop

foreign import _Backdrop :: ∀ a. ReactComponent a