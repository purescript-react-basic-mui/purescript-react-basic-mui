module React.Basic.MUI.Core.Styles.WithStyles where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Styles.CreateMuiTheme (Theme)
import React.Basic.MUI.Core.UseMediaQuery (Options)

type StyleRules classkey props = ActualStyleRules Foreign Foreign

type WithStyles stylesorclasskey includetheme = Foreign

withStyles :: Foreign
withStyles = _withStyles
foreign import _withStyles :: Foreign