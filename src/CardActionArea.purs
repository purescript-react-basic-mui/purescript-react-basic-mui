module React.Basic.MUI.CardActionArea where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.ButtonBase (ButtonBaseTypeMap, ExtendButtonBase)

cardActionArea :: ReactComponent
cardActionArea = _CardActionArea
foreign import _CardActionArea :: ReactComponent

type CardActionAreaClassKey = Foreign

type CardActionAreaProps = SimplifiedPropsOf CardActionArea