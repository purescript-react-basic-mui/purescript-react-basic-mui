module React.Basic.MUI.Core.ButtonGroup where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

buttonGroup :: Foreign
buttonGroup = _ButtonGroup
foreign import _ButtonGroup :: Foreign

type ButtonGroupClassKey = Foreign

type ButtonGroupProps = SimplifiedPropsOf ButtonGroup