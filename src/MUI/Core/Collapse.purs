module MUI.Core.Collapse where

import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

foreign import data Timeout :: Type

timeout :: { auto :: Timeout, number :: Number -> Timeout, record :: { appear :: Number, enter :: Number, exit :: Number } -> Timeout }
timeout = { auto: Unsafe.Coerce.unsafeCoerce "auto", number: Unsafe.Coerce.unsafeCoerce, record: Unsafe.Coerce.unsafeCoerce }

type CollapsePropsOptions componentProps = ( children :: Array React.Basic.JSX, collapsedHeight :: String, component :: React.Basic.ReactComponent {  | componentProps }, timeout :: Timeout | componentProps )

foreign import data CollapseProps :: Type

foreign import _Collapse :: ∀ a. React.Basic.ReactComponent a

collapse :: ∀ required given. Prim.Row.Union given required (CollapsePropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
collapse = React.Basic.element _Collapse

collapse_component :: ∀ required given componentProps. Prim.Row.Union given required (CollapsePropsOptions componentProps) => Record given -> React.Basic.JSX
collapse_component = React.Basic.element _Collapse