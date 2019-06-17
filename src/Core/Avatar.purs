module React.Basic.MUI.Core.Avatar where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

avatar :: Foreign
avatar = _Avatar
foreign import _Avatar :: Foreign

type AvatarClassKey = Foreign

type AvatarProps = SimplifiedPropsOf Avatar