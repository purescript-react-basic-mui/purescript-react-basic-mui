module React.Basic.MUI.Core.BottomNavigationAction where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.ButtonBase (ButtonBaseTypeMap, ExtendButtonBase)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.Events (EventHandler)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

bottomNavigationAction :: EventHandler
bottomNavigationAction = _BottomNavigationAction
foreign import _BottomNavigationAction :: EventHandler

type BottomNavigationActionClassKey = Foreign

type BottomNavigationActionProps = SimplifiedPropsOf BottomNavigationAction