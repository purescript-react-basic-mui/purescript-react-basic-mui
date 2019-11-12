module MUI.Core.Fade where

import Foreign (Foreign) as Foreign
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type FadePropsOptions componentProps = ( ref :: Foreign.Foreign | componentProps )

foreign import data FadeProps :: Type

foreign import data FadePropsPartial :: Type

fadePropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (FadePropsOptions React.Basic.DOM.Props_div) => Record options -> FadePropsPartial
fadePropsPartial = Unsafe.Coerce.unsafeCoerce

foreign import _Fade :: ∀ a. React.Basic.ReactComponent a

fade :: ∀ required given. Prim.Row.Union given required (FadePropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
fade = React.Basic.element _Fade

fade_component :: ∀ required given componentProps. Prim.Row.Union given required (FadePropsOptions componentProps) => Record given -> React.Basic.JSX
fade_component = React.Basic.element _Fade