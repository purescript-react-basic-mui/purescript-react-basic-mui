module MUI.Core.Popper where

import Foreign (Foreign) as Foreign
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type PopperPropsOptions componentProps = ( anchorEl :: Foreign.Foreign, children :: Array React.Basic.JSX, disablePortal :: Boolean, keepMounted :: Boolean, modifiers :: Foreign.Foreign, open :: Boolean, popperOptions :: Foreign.Foreign, popperRef :: Foreign.Foreign, transition :: Boolean | componentProps )

foreign import data PopperProps :: Type

foreign import data PopperPropsPartial :: Type

popperPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (PopperPropsOptions React.Basic.DOM.Props_div) => Record options -> PopperPropsPartial
popperPropsPartial = Unsafe.Coerce.unsafeCoerce

foreign import _Popper :: ∀ a. React.Basic.ReactComponent a

popper :: ∀ required given. Prim.Row.Union given required (PopperPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
popper = React.Basic.element _Popper

popper_component :: ∀ required given componentProps. Prim.Row.Union given required (PopperPropsOptions componentProps) => Record given -> React.Basic.JSX
popper_component = React.Basic.element _Popper