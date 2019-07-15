module MUI.Core.Backdrop where

import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

type BackdropProps =
  ( invisible :: Boolean
  , children :: Array JSX
  , classes :: BackdropClassKey
  , className :: String
  , onClick :: EventHandler
  , open :: Boolean
  , transitionDuration :: { enter :: Number, exit :: Number }
  , ref :: Foreign
  , in :: Boolean
  , timeout :: Number
  , onEnter :: EventHandler
  , onEntering :: EventHandler
  , onEntered :: EventHandler
  , onExit :: EventHandler
  , onExiting :: EventHandler
  , onExited :: EventHandler
  , mountOnEnter :: Boolean
  , unmountOnExit :: Boolean
  , addEndListener :: EventHandler
  )


foreign import data BackdropPropsPartial :: Type
foreign import data BackdropClassKey :: Type

type BackdropClassKeyOptions = 
  ( root :: String
  , invisible :: String
  )

backdropClassKey :: ∀ options options_
  .  Union options options_ BackdropClassKeyOptions
  => Record options
  -> BackdropClassKey
backdropClassKey = unsafeCoerce

backdropPropsPartial :: ∀ props props_
  .  Union props props_ BackdropProps
  => Record props 
  -> BackdropPropsPartial
backdropPropsPartial = unsafeCoerce

backdrop :: ∀ props props_
  .  Union props props_ BackdropProps
  => Record props 
  -> JSX
backdrop = element _Backdrop

foreign import _Backdrop :: ∀ a. ReactComponent a

