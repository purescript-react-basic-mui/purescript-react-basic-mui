module React.Basic.MUI.Core.CardActionArea where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.ButtonBase (ButtonBaseTypeMap, ExtendButtonBase)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

cardActionArea :: Foreign
cardActionArea = _CardActionArea
foreign import _CardActionArea :: Foreign

type CardActionAreaClassKey = Foreign

type CardActionAreaProps = SimplifiedPropsOf CardActionArea