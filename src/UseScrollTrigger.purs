module React.Basic.MUI.UseScrollTrigger where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type UseScrollTriggerOptions_optional =
  ( disableHysteresis :: Boolean
  , target :: Foreign
  , threshold :: Number
  )

foreign import data UseScrollTriggerOptions :: Type 

useScrollTriggerOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (UseScrollTriggerOptions_optional)
  => Record (attrs)
  -> UseScrollTriggerOptions
useScrollTriggerOptions = unsafeCoerce

useScrollTrigger :: Boolean
useScrollTrigger = _useScrollTrigger
foreign import _useScrollTrigger :: Boolean