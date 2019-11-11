module MUI.Core.Backdrop where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Fade (FadePropsOptions) as MUI.Core.Fade
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

foreign import data TransitionDuration :: Type

transitionDuration :: { number :: Number -> TransitionDuration, record :: { appear :: Number, enter :: Number, exit :: Number } -> TransitionDuration }
transitionDuration = { number: Unsafe.Coerce.unsafeCoerce, record: Unsafe.Coerce.unsafeCoerce }

type BackdropPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: BackdropClassKey, invisible :: Boolean, open :: Boolean, transitionDuration :: TransitionDuration | componentProps )

foreign import data BackdropProps :: Type

type BackdropClassKeyGenericOptions a = ( invisible :: a, root :: a )

type BackdropClassKeyOptions  = BackdropClassKeyGenericOptions String

foreign import data BackdropClassKey :: Type

backdropClassKey :: ∀ required given. Prim.Row.Union given required BackdropClassKeyOptions => Record given -> BackdropClassKey
backdropClassKey = Unsafe.Coerce.unsafeCoerce

type BackdropClassKeyOptionsJSS  = BackdropClassKeyGenericOptions MUI.Core.JSS

foreign import data BackdropClassKeyJSS :: Type

backdropClassKeyJSS :: ∀ required given. Prim.Row.Union given required BackdropClassKeyOptionsJSS => Record given -> BackdropClassKeyJSS
backdropClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Backdrop :: ∀ a. React.Basic.ReactComponent a

backdrop :: ∀ required given. Prim.Row.Union given required (BackdropPropsOptions (MUI.Core.Fade.FadePropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
backdrop = React.Basic.element _Backdrop

backdrop_component :: ∀ required given componentProps. Prim.Row.Union given required (BackdropPropsOptions componentProps) => Record given -> React.Basic.JSX
backdrop_component = React.Basic.element _Backdrop
