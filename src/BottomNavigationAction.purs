module React.Basic.MUI.BottomNavigationAction where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.ButtonBase (ButtonBaseTypeMap, ExtendButtonBase)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.Events (EventHandler)

bottomNavigationAction :: EventHandler
bottomNavigationAction = _BottomNavigationAction
foreign import _BottomNavigationAction :: EventHandler

type BottomNavigationActionClassKey = Foreign

type BottomNavigationActionProps = SimplifiedPropsOf BottomNavigationAction