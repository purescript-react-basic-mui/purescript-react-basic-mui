module React.Basic.MUI.Core.Chip where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)
import React.Basic.Events (EventHandler)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

chip :: Foreign
chip = _Chip
foreign import _Chip :: Foreign

type ChipClassKey = Foreign

type ChipProps = SimplifiedPropsOf Chip