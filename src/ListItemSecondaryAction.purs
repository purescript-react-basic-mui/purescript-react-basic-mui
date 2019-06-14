module React.Basic.MUI.ListItemSecondaryAction where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type ListItemSecondaryActionProps  =
  { classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , style :: CSS
  , ref :: Foreign
  }

type ListItemSecondaryActionProps_required =
  ( 
  )

type ListItemSecondaryActionProps_optional =
  ( classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , style :: CSS
  , ref :: Foreign
  )

listItemSecondaryAction :: JSX
listItemSecondaryAction = _ListItemSecondaryAction
foreign import _ListItemSecondaryAction :: JSX