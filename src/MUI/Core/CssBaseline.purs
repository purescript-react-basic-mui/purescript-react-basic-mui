module MUI.Core.CssBaseline where

import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM

type CssBaselinePropsOptions componentProps = ( children :: Array React.Basic.JSX | componentProps )

foreign import data CssBaselineProps :: Type

foreign import data CssBaselinePropsPartial :: Type

foreign import _CssBaseline :: ∀ a. React.Basic.ReactComponent a

cssBaseline :: ∀ required given. Prim.Row.Union given required (CssBaselinePropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
cssBaseline = React.Basic.element _CssBaseline

cssBaseline_component :: ∀ required given componentProps. Prim.Row.Union given required (CssBaselinePropsOptions componentProps) => Record given -> React.Basic.JSX
cssBaseline_component = React.Basic.element _CssBaseline