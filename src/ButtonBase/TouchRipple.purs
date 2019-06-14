module React.Basic.MUI.ButtonBase.TouchRipple where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)

type TouchRippleProps = Foreign

type TouchRippleClassKey = Foreign

touchRipple
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TouchRippleProps_optional)
  => Record (attrs)
  -> JSX
touchRipple = element _TouchRipple
foreign import _TouchRipple :: forall a. ReactComponent a 