module MUI.Core.Styles where

import MUI.Core (JSS)
import MUI.Core.Styles.CreateMuiTheme (Theme)
import React.Basic (ReactComponent)

foreign import withStyles ∷ ∀ props
  . (Theme → JSS)
  → ReactComponent props
  → ReactComponent props

