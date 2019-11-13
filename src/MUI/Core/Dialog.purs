module MUI.Core.Dialog where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Modal (ModalPropsOptions) as MUI.Core.Modal
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data TransitionDuration :: Type

transitionDuration :: { number :: Number -> TransitionDuration, record :: { appear :: Number, enter :: Number, exit :: Number } -> TransitionDuration }
transitionDuration = { number: Unsafe.Coerce.unsafeCoerce, record: Unsafe.Coerce.unsafeCoerce }

foreign import data Scroll :: Type

scroll :: { body :: Scroll, paper :: Scroll }
scroll = { body: Unsafe.Coerce.unsafeCoerce "body", paper: Unsafe.Coerce.unsafeCoerce "paper" }

foreign import data MaxWidth :: Type

maxWidth :: { "false" :: MaxWidth, lg :: MaxWidth, md :: MaxWidth, sm :: MaxWidth, xl :: MaxWidth, xs :: MaxWidth }
maxWidth = { "false": Unsafe.Coerce.unsafeCoerce false, lg: Unsafe.Coerce.unsafeCoerce "lg", md: Unsafe.Coerce.unsafeCoerce "md", sm: Unsafe.Coerce.unsafeCoerce "sm", xl: Unsafe.Coerce.unsafeCoerce "xl", xs: Unsafe.Coerce.unsafeCoerce "xs" }

instance eqMaxWidth :: Eq MaxWidth where
  eq = Unsafe.Reference.unsafeRefEq

instance eqScroll :: Eq Scroll where
  eq = Unsafe.Reference.unsafeRefEq

type DialogPropsOptions componentProps = ( "aria-describedby" :: String, "aria-labelledby" :: String, children :: Array React.Basic.JSX, classes :: DialogClassKey, fullScreen :: Boolean, fullWidth :: Boolean, maxWidth :: MaxWidth, onEnter :: React.Basic.Events.EventHandler, onEntered :: React.Basic.Events.EventHandler, onEntering :: React.Basic.Events.EventHandler, onExit :: React.Basic.Events.EventHandler, onExited :: React.Basic.Events.EventHandler, onExiting :: React.Basic.Events.EventHandler, scroll :: Scroll, transitionDuration :: TransitionDuration | componentProps )

foreign import data DialogProps :: Type

foreign import data DialogPropsPartial :: Type

dialogPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (DialogPropsOptions (MUI.Core.Modal.ModalPropsOptions React.Basic.DOM.Props_div)) => Record options -> DialogPropsPartial
dialogPropsPartial = Unsafe.Coerce.unsafeCoerce

type DialogClassKeyGenericOptions a = ( container :: a, paper :: a, paperFullScreen :: a, paperFullWidth :: a, paperScrollBody :: a, paperScrollPaper :: a, paperWidthFalse :: a, paperWidthLg :: a, paperWidthMd :: a, paperWidthSm :: a, paperWidthXl :: a, paperWidthXs :: a, root :: a, scrollBody :: a, scrollPaper :: a )

type DialogClassKeyOptions  = DialogClassKeyGenericOptions String

foreign import data DialogClassKey :: Type

dialogClassKey :: ∀ required given. Prim.Row.Union given required DialogClassKeyOptions => Record given -> DialogClassKey
dialogClassKey = Unsafe.Coerce.unsafeCoerce

type DialogClassKeyOptionsJSS  = DialogClassKeyGenericOptions MUI.Core.JSS

foreign import data DialogClassKeyJSS :: Type

dialogClassKeyJSS :: ∀ required given. Prim.Row.Union given required DialogClassKeyOptionsJSS => Record given -> DialogClassKeyJSS
dialogClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Dialog :: ∀ a. React.Basic.ReactComponent a

dialog :: ∀ required given. Prim.Row.Union given required (DialogPropsOptions (MUI.Core.Modal.ModalPropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
dialog = React.Basic.element _Dialog

dialog_component :: ∀ required given componentProps. Prim.Row.Union given required (DialogPropsOptions componentProps) => Record given -> React.Basic.JSX
dialog_component = React.Basic.element _Dialog

dialogWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (DialogPropsOptions (MUI.Core.Modal.ModalPropsOptions React.Basic.DOM.Props_div)) => Prim.Row.Union jss jss_ DialogClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
dialogWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Dialog)