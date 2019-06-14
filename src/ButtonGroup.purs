module React.Basic.MUI.ButtonGroup where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, ReactComponent)

buttonGroup :: Foreign
buttonGroup = _ButtonGroup
foreign import _ButtonGroup :: Foreign

type ButtonGroupClassKey = Foreign

type ButtonGroupProps = SimplifiedPropsOf ButtonGroup