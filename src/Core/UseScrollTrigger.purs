module React.Basic.MUI.Core.UseScrollTrigger where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type UseScrollTriggerOptions_optional =
  ( disableHysteresis :: Boolean
  , target :: Foreign
  , threshold :: Number
  )

foreign import data UseScrollTriggerOptions :: Type 



useScrollTrigger :: Boolean
useScrollTrigger = _useScrollTrigger
foreign import _useScrollTrigger :: Boolean