module React.Basic.MUI.Core.StepButton where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.ButtonBase (ButtonBaseTypeMap, ExtendButtonBase)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Core.Stepper (Orientation)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

type StepButtonIcon = Foreign

stepButton :: Foreign
stepButton = _StepButton
foreign import _StepButton :: Foreign

type StepButtonClasskey = Foreign

type StepButtonProps = SimplifiedPropsOf StepButton