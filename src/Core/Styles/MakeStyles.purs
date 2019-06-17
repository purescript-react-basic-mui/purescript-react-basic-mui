module React.Basic.MUI.Core.Styles.MakeStyles where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Styles.CreateMuiTheme (Theme)

makeStyles :: Foreign
makeStyles = _makeStyles
foreign import _makeStyles :: Foreign