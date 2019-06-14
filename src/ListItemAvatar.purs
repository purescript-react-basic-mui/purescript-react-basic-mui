module React.Basic.MUI.ListItemAvatar where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type ListItemAvatarProps  =
  { classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , style :: CSS
  , ref :: Foreign
  }

type ListItemAvatarProps_required =
  ( 
  )

type ListItemAvatarProps_optional =
  ( classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , style :: CSS
  , ref :: Foreign
  )

listItemAvatar :: JSX
listItemAvatar = _ListItemAvatar
foreign import _ListItemAvatar :: JSX