module React.Basic.MUI.Tab where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.ButtonBase (ExtendButtonBase)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.Events (EventHandler)
import React.Basic.DOM.Internal (CSS)

tab :: EventHandler
tab = _Tab
foreign import _Tab :: EventHandler

type TabClassKey = Foreign

type TabProps = SimplifiedPropsOf Tab