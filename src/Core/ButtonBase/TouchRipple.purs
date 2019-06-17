module React.Basic.MUI.Core.ButtonBase.TouchRipple where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Index (StandardProps)
import React.Basic (element, ReactComponent, JSX)

type TouchRippleProps = Foreign

type TouchRippleClassKey = Foreign

touchRipple
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TouchRippleProps_optional )
  => Record (attrs)
  -> JSX
touchRipple = element _TouchRipple
foreign import _TouchRipple :: forall a. ReactComponent a 