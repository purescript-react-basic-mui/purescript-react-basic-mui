module React.Basic.MUI.ListItem where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type ListItemTypeMap  p d =
  { props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  }

type ListItemTypeMap_required p d =
  ( props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  )

type ListItemTypeMap_optional p d =
  ( 
  )

listItem :: Foreign
listItem = _ListItem
foreign import _ListItem :: Foreign