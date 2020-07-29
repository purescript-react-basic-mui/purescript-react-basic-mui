module MUI.Core.Styles.MuiThemeProvider where

import MUI.Core.Styles.Types (Theme)
import React.Basic (JSX, ReactComponent, element)

type ThemeProviderProps
  = { children :: JSX
    , theme :: Theme
    }

foreign import _MuiThemeProvider :: ReactComponent ThemeProviderProps

muiThemeProvider :: { children :: JSX, theme :: Theme } -> JSX
muiThemeProvider = element _MuiThemeProvider
