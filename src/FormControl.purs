module React.Basic.MUI.FormControl where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, ReactComponent)
import React.Basic.Events (EventHandler)

formControl :: Foreign
formControl = _FormControl
foreign import _FormControl :: Foreign

type FormControlClassKey = Foreign

type FormControlProps = SimplifiedPropsOf FormControl