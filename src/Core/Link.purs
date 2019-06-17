module React.Basic.MUI.Core.Link where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Typography (TypographyProps)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

link :: Foreign
link = _Link
foreign import _Link :: Foreign

type LinkClassKey = Foreign

type LinkBaseProps = Foreign

type LinkProps = SimplifiedPropsOf Link