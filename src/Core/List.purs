module React.Basic.MUI.Core.List where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

list :: Foreign
list = _List
foreign import _List :: Foreign

type ListClassKey = Foreign

type ListProps = SimplifiedPropsOf List