module MUI.Core.Drawer where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Modal (ModalPropsOptions, ModalPropsPartial) as MUI.Core.Modal
import MUI.Core.Slide (SlidePropsPartial) as MUI.Core.Slide
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { permanent :: Variant, persistent :: Variant, temporary :: Variant }
variant = { permanent: Unsafe.Coerce.unsafeCoerce "permanent", persistent: Unsafe.Coerce.unsafeCoerce "persistent", temporary: Unsafe.Coerce.unsafeCoerce "temporary" }

foreign import data TransitionDuration :: Type

transitionDuration :: { number :: Number -> TransitionDuration, record :: { appear :: Number, enter :: Number, exit :: Number } -> TransitionDuration }
transitionDuration = { number: Unsafe.Coerce.unsafeCoerce, record: Unsafe.Coerce.unsafeCoerce }

foreign import data Anchor :: Type

anchor :: { bottom :: Anchor, left :: Anchor, right :: Anchor, top :: Anchor }
anchor = { bottom: Unsafe.Coerce.unsafeCoerce "bottom", left: Unsafe.Coerce.unsafeCoerce "left", right: Unsafe.Coerce.unsafeCoerce "right", top: Unsafe.Coerce.unsafeCoerce "top" }

instance eqAnchor :: Eq Anchor where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type DrawerPropsOptions componentProps = ( "ModalProps" :: MUI.Core.Modal.ModalPropsPartial, "PaperProps" :: MUI.Core.Modal.ModalPropsPartial, "SlideProps" :: MUI.Core.Slide.SlidePropsPartial, anchor :: Anchor, children :: Array React.Basic.JSX, classes :: DrawerClassKey, elevation :: Number, onClose :: React.Basic.Events.EventHandler, onEnter :: React.Basic.Events.EventHandler, onEntered :: React.Basic.Events.EventHandler, onEntering :: React.Basic.Events.EventHandler, onExit :: React.Basic.Events.EventHandler, onExited :: React.Basic.Events.EventHandler, onExiting :: React.Basic.Events.EventHandler, open :: Boolean, transitionDuration :: TransitionDuration, variant :: Variant | componentProps )

foreign import data DrawerProps :: Type

foreign import data DrawerPropsPartial :: Type

drawerPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (DrawerPropsOptions (MUI.Core.Modal.ModalPropsOptions React.Basic.DOM.Props_div)) => Record options -> DrawerPropsPartial
drawerPropsPartial = Unsafe.Coerce.unsafeCoerce

type DrawerClassKeyGenericOptions a = ( docked :: a, modal :: a, paper :: a, paperAnchorBottom :: a, paperAnchorDockedBottom :: a, paperAnchorDockedLeft :: a, paperAnchorDockedRight :: a, paperAnchorDockedTop :: a, paperAnchorLeft :: a, paperAnchorRight :: a, paperAnchorTop :: a, root :: a )

type DrawerClassKeyOptions  = DrawerClassKeyGenericOptions String

foreign import data DrawerClassKey :: Type

drawerClassKey :: ∀ required given. Prim.Row.Union given required DrawerClassKeyOptions => Record given -> DrawerClassKey
drawerClassKey = Unsafe.Coerce.unsafeCoerce

type DrawerClassKeyOptionsJSS  = DrawerClassKeyGenericOptions MUI.Core.JSS

foreign import data DrawerClassKeyJSS :: Type

drawerClassKeyJSS :: ∀ required given. Prim.Row.Union given required DrawerClassKeyOptionsJSS => Record given -> DrawerClassKeyJSS
drawerClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Drawer :: ∀ a. React.Basic.ReactComponent a

drawer :: ∀ required given. Prim.Row.Union given required (DrawerPropsOptions (MUI.Core.Modal.ModalPropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
drawer = React.Basic.element _Drawer

drawer_component :: ∀ required given componentProps. Prim.Row.Union given required (DrawerPropsOptions componentProps) => Record given -> React.Basic.JSX
drawer_component = React.Basic.element _Drawer