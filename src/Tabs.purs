module React.Basic.MUI.Tabs where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


tabs :: Foreign
tabs = _Tabs
foreign import _Tabs :: Foreign

type TabsActions  =
  { updateIndicator :: Foreign
  }

type TabsActions_required =
  ( updateIndicator :: Foreign
  )

type TabsActions_optional =
  ( 
  )