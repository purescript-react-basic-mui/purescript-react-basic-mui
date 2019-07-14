module MUI.Core.ButtonBase where

import Prelude

import Data.Maybe (Maybe)
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
  ( action :: Maybe (Ref ButtonBaseActions)
  , buttonRef :: Maybe (Ref Foreign)
  , centerRipple :: Maybe Boolean
  , classes :: ButtonBaseClassKey
  , component :: Maybe String
  , disabled :: Maybe Boolean
  , disableRipple :: Maybe Boolean
  , disableTouchRipple :: Maybe Boolean
  , focusRipple :: Maybe Boolean
  , focusVisibleClassName :: Maybe String
  , onFocusVisible :: Maybe EventHandler
  , "TouchRippleProps" :: Maybe TouchRippleProps
  , type :: Maybe String
  )

type ButtonBaseClassKeyOptions =
  ( root :: String
  , disabled :: String
  , focusVisible :: String
  )

foreign import data ButtonBaseClassKey :: Type


buttonBaseClassKey
  :: ∀ options options_
  . Union options options_ ButtonBaseClassKeyOptions
  => Record options
  -> ButtonBaseClassKey
buttonBaseClassKey = unsafeCoerce

buttonBase
  :: ∀ props props_
  . Union props props_ ButtonBaseProps
  => Record props 
  -> JSX
buttonBase = element _ButtonBase


foreign import _ButtonBase :: ∀ a. ReactComponent a