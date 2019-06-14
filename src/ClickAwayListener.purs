module React.Basic.MUI.ClickAwayListener where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type ClickAwayListenerProps  =
  { children :: JSX
  , mouseEvent :: Foreign
  , onClickAway :: Foreign
  , touchEvent :: Foreign
  }

type ClickAwayListenerProps_required =
  ( children :: JSX
  , onClickAway :: Foreign
  )

type ClickAwayListenerProps_optional =
  ( mouseEvent :: Foreign
  , touchEvent :: Foreign
  )

clickAwayListener :: JSX
clickAwayListener = _ClickAwayListener
foreign import _ClickAwayListener :: JSX