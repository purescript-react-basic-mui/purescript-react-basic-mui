module React.Basic.MUI.Divider where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, ReactComponent)

divider :: Foreign
divider = _Divider
foreign import _Divider :: Foreign

type DividerClassKey = Foreign

type DividerProps = SimplifiedPropsOf Divider