module MUI.Core.Modal where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Foreign.Object (Object)
import MUI.Core (JSS)
import MUI.Core.Internal (backdropComponent, onBackdropClick, onClose, onEscapeKeyDown, onRendered, toInternalChildren)
import MUI.Core.Modal.ModalManager (ModalManager)
import React.Basic (Component, JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type BackdropProps = Object Foreign

type ModalProps =
  ( "BackdropComponent" :: Maybe (Component BackdropProps)
  , "BackdropProps" :: Maybe BackdropProps
  , children :: Maybe (Array JSX)
  , className :: Maybe String
  , classes :: ModalClassKey
  , closeAfterTransition :: Maybe Boolean
  , container :: Maybe Foreign
  , disableAutoFocus :: Maybe Boolean
  , disableBackdropClick :: Maybe Boolean
  , disableEnforceFocus :: Maybe Boolean
  , disableEscapeKeyDown :: Maybe Boolean
  , disablePortal :: Maybe Boolean
  , disableRestoreFocus :: Maybe Boolean
  , hideBackdrop :: Maybe Boolean
  , manager :: Maybe ModalManager
  , onBackdropClick :: Maybe EventHandler
  , onClose :: Maybe EventHandler
  , onEscapeKeyDown :: Maybe EventHandler
  , onRendered :: Maybe EventHandler
  , open :: Boolean
  )

type ModalClassKey = 
  { root :: Maybe JSS
  , hidden :: Maybe JSS
  }

classes :: ModalClassKey
classes =
  { root : Nothing
  , hidden : Nothing
  }

modalProps :: { | ModalProps }
modalProps = 
  { "BackdropComponent" : Nothing
  , "BackdropProps" : Nothing
  , children : Nothing
  , className : Nothing
  , classes
  , closeAfterTransition : Just false
  , container : Nothing
  , disableAutoFocus : Just false
  , disableBackdropClick : Just false
  , disableEnforceFocus : Just false
  , disableEscapeKeyDown : Just false
  , disablePortal : Just false
  , disableRestoreFocus : Just false
  , hideBackdrop : Just false
  , manager : Nothing
  , onBackdropClick : Nothing
  , onClose : Nothing
  , onEscapeKeyDown : Nothing
  , onRendered : Nothing
  , open : false
  }

modal :: { | ModalProps } -> JSX
modal props = do 
  element _Modal 
    $ unsafeCoerce 
    $ (write ((toInternalChildren <<< backdropComponent <<< onBackdropClick <<< onEscapeKeyDown <<< onRendered <<< onClose)  props))

foreign import _Modal :: ∀ a. ReactComponent a 