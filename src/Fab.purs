module React.Basic.MUI.Fab where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.ButtonBase (ExtendButtonBase)

fab :: ReactComponent
fab = _Fab
foreign import _Fab :: ReactComponent

type FabProps = SimplifiedPropsOf Fab

type FabClassKey = Foreign