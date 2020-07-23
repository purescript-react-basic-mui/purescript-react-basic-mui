module MUI.Core.Styles.MuiThemeProvider where

import Foreign (Foreign, unsafeToForeign)
import MUI.Core.Styles.Types (Theme)
import React.Basic (JSX)

type ThemeProviderProps
  = { children :: JSX
    , theme :: Theme
    }

muiThemeProvider :: ThemeProviderProps -> JSX
muiThemeProvider props = _MuiThemeProvider (unsafeToForeign props)

foreign import _MuiThemeProvider :: Foreign -> JSX
