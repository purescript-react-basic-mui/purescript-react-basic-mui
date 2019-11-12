module MUI.Core.Modal where

import MUI.Core.Backdrop (BackdropPropsOptions) as MUI.Core.Backdrop
import MUI.Core.Modal.ModalManager (ModalManager) as MUI.Core.Modal.ModalManager
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type ModalPropsOptions componentProps = ( "BackdropComponent" :: ∀ a. React.Basic.ReactComponent a, "BackdropProps" :: ∀ required given. Prim.Row.Union given required (MUI.Core.Backdrop.BackdropPropsOptions React.Basic.DOM.Props_div) => Record given, children :: Array React.Basic.JSX, closeAfterTransition :: Boolean, disableAutoFocus :: Boolean, disableBackdropClick :: Boolean, disableEnforceFocus :: Boolean, disableEscapeKeyDown :: Boolean, disablePortal :: Boolean, disableRestoreFocus :: Boolean, disableScrollLock :: Boolean, hideBackdrop :: Boolean, keepMounted :: Boolean, manager :: MUI.Core.Modal.ModalManager.ModalManager, onBackdropClick :: React.Basic.Events.EventHandler, onClose :: React.Basic.Events.EventHandler, onEscapeKeyDown :: React.Basic.Events.EventHandler, onRendered :: React.Basic.Events.EventHandler, open :: Boolean | componentProps )

foreign import data ModalProps :: Type

foreign import data ModalPropsPartial :: Type

modalPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ModalPropsOptions React.Basic.DOM.Props_div) => Record options -> ModalPropsPartial
modalPropsPartial = Unsafe.Coerce.unsafeCoerce

foreign import _Modal :: ∀ a. React.Basic.ReactComponent a

modal :: ∀ required given. Prim.Row.Union given required (ModalPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
modal = React.Basic.element _Modal

modal_component :: ∀ required given componentProps. Prim.Row.Union given required (ModalPropsOptions componentProps) => Record given -> React.Basic.JSX
modal_component = React.Basic.element _Modal