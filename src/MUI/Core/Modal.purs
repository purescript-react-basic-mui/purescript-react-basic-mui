module MUI.Core.Modal where

import Foreign (Foreign)
import Foreign.Object (Object)
import MUI.Core.Modal.ModalManager (ModalManager)
import React.Basic (Component, JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type BackdropProps = Object Foreign

type ModalProps =
  ( "BackdropComponent" :: Component BackdropProps
  , "BackdropProps" :: BackdropProps
  , children :: Array JSX
  , className :: String
  , classes :: ModalClassKey
  , closeAfterTransition :: Boolean
  , container :: Foreign
  , disableAutoFocus :: Boolean
  , disableBackdropClick :: Boolean
  , disableEnforceFocus :: Boolean
  , disableEscapeKeyDown :: Boolean
  , disablePortal :: Boolean
  , disableRestoreFocus :: Boolean
  , hideBackdrop :: Boolean
  , manager :: ModalManager
  , onBackdropClick :: EventHandler
  , onClose :: EventHandler
  , onEscapeKeyDown :: EventHandler
  , onRendered :: EventHandler
  , open :: Boolean
  )

foreign import data ModalClassKey :: Type

type ModalClassKeyOptions = 
  ( root :: String
  , hidden :: String
  )

modalClassKey 
  :: ∀ options options_
  . Union options options_ ModalClassKeyOptions
  => Record options
  -> ModalClassKey
modalClassKey = unsafeCoerce

modal
  :: ∀ props props_
  . Union props props_ ModalProps
  => Record props 
  -> JSX
modal = element _Modal

foreign import _Modal :: ∀ a. ReactComponent a 