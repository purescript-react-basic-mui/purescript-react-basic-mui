module React.Basic.MUI.List where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, ReactComponent)

list :: Foreign
list = _List
foreign import _List :: Foreign

type ListClassKey = Foreign

type ListProps = SimplifiedPropsOf List