module React.Basic.MUI.Core.Divider where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

divider :: Foreign
divider = _Divider
foreign import _Divider :: Foreign

type DividerClassKey = Foreign

type DividerProps = SimplifiedPropsOf Divider