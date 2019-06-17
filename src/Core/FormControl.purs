module React.Basic.MUI.Core.FormControl where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.Events (EventHandler)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

formControl :: Foreign
formControl = _FormControl
foreign import _FormControl :: Foreign

type FormControlClassKey = Foreign

type FormControlProps = SimplifiedPropsOf FormControl