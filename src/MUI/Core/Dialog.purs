module MUI.Core.Dialog where

import Effect (Effect) as Effect
import MUI.Core (JSS) as MUI.Core
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
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
maxWidth = { false: Unsafe.Coerce.unsafeCoerce false, lg: Unsafe.Coerce.unsafeCoerce "lg", md: Unsafe.Coerce.unsafeCoerce "md", sm: Unsafe.Coerce.unsafeCoerce "sm", xl: Unsafe.Coerce.unsafeCoerce "xl", xs: Unsafe.Coerce.unsafeCoerce "xs" }

instance eqMaxWidth :: Eq MaxWidth where
  eq = Unsafe.Reference.unsafeRefEq

instance eqScroll :: Eq Scroll where
  eq = Unsafe.Reference.unsafeRefEq

type DialogPropsOptions componentProps = ( "aria-describedby" :: String, "aria-labelledby" :: String, children :: Array React.Basic.JSX, classes :: DialogClassKey, disableBackdropClick :: Boolean, disableEscapeKeyDown :: Boolean, fullScreen :: Boolean, fullWidth :: Boolean, maxWidth :: MaxWidth, onBackdropClick :: Effect.Effect Unit, onClose :: Effect.Effect Unit, onEnter :: Effect.Effect Unit, onEntered :: Effect.Effect Unit, onEntering :: Effect.Effect Unit, onEscapeKeyDown :: Effect.Effect Unit, onExit :: Effect.Effect Unit, onExited :: Effect.Effect Unit, onExiting :: Effect.Effect Unit, open :: Boolean, scroll :: Scroll, transitionDuration :: TransitionDuration | componentProps )

foreign import data DialogProps :: Type

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

dialog :: ∀ required given. Prim.Row.Union given required (DialogPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
dialog = React.Basic.element _Dialog

dialog_component :: ∀ required given componentProps. Prim.Row.Union given required (DialogPropsOptions componentProps) => Record given -> React.Basic.JSX
dialog_component = React.Basic.element _Dialog