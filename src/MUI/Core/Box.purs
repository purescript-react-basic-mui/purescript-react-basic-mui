module MUI.Core.Box where

import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM

type BoxPropsOptions componentProps = ( children :: Array React.Basic.JSX, clone :: Boolean, component :: React.Basic.ReactComponent {  | componentProps }, css :: MUI.Core.JSS | componentProps )

foreign import data BoxProps :: Type

foreign import data BoxPropsPartial :: Type

foreign import _Box :: ∀ a. React.Basic.ReactComponent a

box :: ∀ required given. Prim.Row.Union given required (BoxPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
box = React.Basic.element _Box

box_component :: ∀ required given componentProps. Prim.Row.Union given required (BoxPropsOptions componentProps) => Record given -> React.Basic.JSX
box_component = React.Basic.element _Box