module React.Basic.MUI.Core.Tab where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.ButtonBase (ExtendButtonBase)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.Events (EventHandler)
import React.Basic.DOM.Internal (CSS)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

tab :: EventHandler
tab = _Tab
foreign import _Tab :: EventHandler

type TabClassKey = Foreign

type TabProps = SimplifiedPropsOf Tab