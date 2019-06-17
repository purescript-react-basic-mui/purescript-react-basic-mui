module React.Basic.MUI.Core.Fab where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.ButtonBase (ExtendButtonBase)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

fab :: Foreign
fab = _Fab
foreign import _Fab :: Foreign

type FabProps = SimplifiedPropsOf Fab

type FabClassKey = Foreign