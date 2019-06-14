module React.Basic.MUI.StepButton where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.ButtonBase (ButtonBaseTypeMap, ExtendButtonBase)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Stepper (Orientation)

type StepButtonIcon = Foreign

stepButton :: ReactComponent
stepButton = _StepButton
foreign import _StepButton :: ReactComponent

type StepButtonClasskey = Foreign

type StepButtonProps = SimplifiedPropsOf StepButton