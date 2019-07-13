module MUI.Core.ButtonBase where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Foreign (Foreign)
import Foreign.Object (Object)
import MUI.Core.Internal (action, buttonRef, onFocusVisible, toInternalChildren)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (JSX, ReactComponent, Ref, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type ButtonBaseActions =
  { focusVisible :: Effect Unit
  }

type TouchRippleProps = Object Foreign

type ButtonBaseProps =
  ( action :: Maybe (Ref ButtonBaseActions)
  , buttonRef :: Maybe (Ref Foreign)
  , centerRipple :: Maybe Boolean
  , children :: Maybe (Array JSX)
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

buttonBaseProps :: { | ButtonBaseProps }
buttonBaseProps =
  { action : Nothing
  , buttonRef : Nothing
  , centerRipple : Just false
  , children : Nothing
  , classes
  , component : Just "button"
  , disabled : Nothing
  , disableRipple : Just false
  , disableTouchRipple : Just false
  , focusRipple : Just false
  , focusVisibleClassName : Nothing
  , onFocusVisible : Nothing
  , "TouchRippleProps" : Nothing
  , type : Just "button"
  }


type ButtonBaseClassKey =
  { root :: Maybe String
  , disabled :: Maybe String
  , focusVisible :: Maybe String
  }

classes :: ButtonBaseClassKey
classes = 
  { root : Nothing
  , disabled : Nothing
  , focusVisible : Nothing
  }

buttonBase :: { | ButtonBaseProps } -> JSX
buttonBase props = do
  let foreignProps = write $ (action <<< buttonRef <<< onFocusVisible <<< toInternalChildren) props
  element _ButtonBase (unsafeCoerce foreignProps)

foreign import _ButtonBase :: ∀ a. ReactComponent a