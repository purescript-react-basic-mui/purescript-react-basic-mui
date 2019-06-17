module React.Basic.MUI.Core.IconButton where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.ButtonBase (ExtendButtonBase)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

iconButton :: Foreign
iconButton = _IconButton
foreign import _IconButton :: Foreign

type IconButtonClassKey = Foreign

type IconButtonProps = SimplifiedPropsOf IconButton