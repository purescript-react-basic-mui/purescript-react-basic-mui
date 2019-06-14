module React.Basic.MUI.Avatar where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, ReactComponent)

avatar :: Foreign
avatar = _Avatar
foreign import _Avatar :: Foreign

type AvatarClassKey = Foreign

type AvatarProps = SimplifiedPropsOf Avatar