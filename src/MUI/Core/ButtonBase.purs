module MUI.Core.ButtonBase where

import Prelude

import Effect (Effect)
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX, ReactComponent, Ref, element)
import Unsafe.Coerce (unsafeCoerce)

type ButtonBaseActions =
  { focusVisible :: Effect Unit
  }

type TouchRippleProps = Object Foreign

type ButtonBaseProps =
  ( action :: Ref ButtonBaseActions
  , buttonRef :: Ref Foreign
  , centerRipple :: Boolean
  , classes :: ButtonBaseClassKey
  , component :: String
  , disabled :: Boolean
  , disableRipple :: Boolean
  , disableTouchRipple :: Boolean
  , focusRipple :: Boolean
  , focusVisibleClassName :: String
  , onFocusVisible :: EventHandler
  , "TouchRippleProps" :: TouchRippleProps
  , type :: String
  )

type ButtonBaseClassKeyOptions =
  ( root :: String
  , disabled :: String
  , focusVisible :: String
  )

foreign import data ButtonBasePropsPartial :: Type

foreign import data ButtonBaseClassKey :: Type

buttonBaseClassKey :: ∀ options options_
  . Union options options_ ButtonBaseClassKeyOptions
  => Record options
  -> ButtonBaseClassKey
buttonBaseClassKey = unsafeCoerce

buttonBasePartial :: ∀ props props_
  . Union props props_ ButtonBaseProps
  => Record props 
  -> ButtonBasePropsPartial
buttonBasePartial = unsafeCoerce

buttonBase :: ∀ props props_
  . Union props props_ ButtonBaseProps
  => Record props 
  -> JSX
buttonBase = element _ButtonBase


foreign import _ButtonBase :: ∀ a. ReactComponent a