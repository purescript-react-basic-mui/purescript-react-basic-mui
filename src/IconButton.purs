module React.Basic.MUI.IconButton where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.ButtonBase (ExtendButtonBase)

iconButton :: ReactComponent
iconButton = _IconButton
foreign import _IconButton :: ReactComponent

type IconButtonClassKey = Foreign

type IconButtonProps = SimplifiedPropsOf IconButton