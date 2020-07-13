module MUI.Core.CssBaseline where

import React.Basic (JSX, ReactComponent, element)

foreign import _CssBaseline :: ReactComponent {}

cssBaseline :: JSX
cssBaseline = element _CssBaseline {}
