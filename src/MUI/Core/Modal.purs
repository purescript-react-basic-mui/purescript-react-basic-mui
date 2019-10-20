module MUI.Core.Modal where

import Foreign (Foreign)
import Foreign.Object (Object)
import MUI.Core (JSS)
import MUI.Core.Modal.ModalManager (ModalManager)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

type BackdropProps = Object Foreign

type ModalProps componentProps =
  --( "BackdropComponent" :: Component { | BackdropPropsOptions }
  --, "BackdropProps" :: BackdropProps
  ( children :: Array JSX
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
  | componentProps
  )

foreign import data ModalClassKey :: Type
foreign import data ModalClassKeyJSS :: Type
foreign import data ModalPropsPartial :: Type

type ModalClassKeyOptionsJSS = ModalClassKeyOptionsR JSS
type ModalClassKeyOptions = ModalClassKeyOptionsR String
type ModalClassKeyOptionsR a = 
  ( root :: a 
  , hidden :: a 
  )

modalClassKey :: ∀ options options_
  . Union options options_ ModalClassKeyOptions
  => Record options
  -> ModalClassKey
modalClassKey = unsafeCoerce

modalClassKeyJSS :: ∀ options options_
  . Union options options_ ModalClassKeyOptionsJSS
  => Record options
  -> ModalClassKeyJSS
modalClassKeyJSS = unsafeCoerce

modalPropsPartial :: ∀ options options_
  . Union options options_ (ModalProps Props_div)
  => Record options
  -> ModalPropsPartial
modalPropsPartial = unsafeCoerce

modal :: ∀ props props_
  . Union props props_ (ModalProps Props_div)
  => Record props 
  -> JSX
modal = element _Modal

foreign import _Modal :: ∀ a. ReactComponent a 
