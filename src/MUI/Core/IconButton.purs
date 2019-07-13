module MUI.Core.IconButton where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import MUI.Core.ButtonBase (ButtonBaseActions, TouchRippleProps)
import MUI.Core.Internal (action, buttonRef, onFocusVisible, toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import React.Basic.Hooks (Ref)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type IconButtonProps =
  ( children :: Maybe (Array JSX)
  , classes :: IconButtonClassKey
  , color :: Maybe String
  , edge :: Maybe String
  , size :: Maybe String
  , action :: Maybe (Ref ButtonBaseActions)
  , buttonRef :: Maybe (Ref Foreign)
  , centerRipple :: Maybe Boolean
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
  

iconButtonProps :: { | IconButtonProps }
iconButtonProps = 
  { action : Nothing
  , buttonRef : Nothing
  , centerRipple : Just false
  , children : Nothing
  , classes
  , color : Just "default"
  , component : Just "button"
  , disabled : Nothing
  , disableRipple : Just false
  , disableTouchRipple : Just false
  , focusRipple : Just false
  , focusVisibleClassName : Nothing
  , onFocusVisible : Nothing
  , "TouchRippleProps" : Nothing
  , type : Just "button"
  , edge : Just "false"
  , size : Just "medium"
  }

type IconButtonClassKey =
  { root :: Maybe String
  , edgeStart :: Maybe String
  , edgeEnd :: Maybe String
  , colorInherit :: Maybe String
  , colorPrimary :: Maybe String
  , colorSecondary :: Maybe String
  , disabled :: Maybe String
  , sizeSmall :: Maybe String
  , label :: Maybe String
  }

classes :: IconButtonClassKey
classes = 
  { root : Nothing
  , edgeStart : Nothing
  , edgeEnd : Nothing
  , colorInherit : Nothing
  , colorPrimary : Nothing
  , colorSecondary : Nothing
  , disabled : Nothing
  , sizeSmall : Nothing
  , label : Nothing
  }

iconButton :: { | IconButtonProps } -> JSX
iconButton props = do
  let foreignProps = write $ (action <<< buttonRef <<< onFocusVisible <<< toInternalChildren) props
  element _IconButton (unsafeCoerce foreignProps)


foreign import _IconButton :: ∀ a. ReactComponent a