module React.Basic.MUI.Chip where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX, ReactComponent)
import React.Basic.Events (EventHandler)

chip :: Foreign
chip = _Chip
foreign import _Chip :: Foreign

type ChipClassKey = Foreign

type ChipProps = SimplifiedPropsOf Chip