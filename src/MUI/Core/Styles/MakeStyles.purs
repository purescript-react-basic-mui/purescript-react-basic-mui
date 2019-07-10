module MUI.Core.Styles.MakeStyles where

import Foreign.Object (Object)
import MUI.Core (JSS)
import MUI.Core.Styles.CreateMuiTheme (Theme)
import React.Basic.Hooks (Hook)

foreign import data UseStyles :: Type -> Type
foreign import makeStyles :: (Theme -> JSS) -> Hook UseStyles (Object String)