module MUI.Core.Styles.WithStyles where

import MUI.Core (JSS)
import MUI.Core.Styles.Types (Theme)
import React.Basic (ReactComponent)

foreign import withStyles :: ∀ props
  . (Theme -> JSS)
  -> ReactComponent props
  -> ReactComponent props

