module React.Basic.MUI.Collapse where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type CollapseProps  =
  { children :: JSX
  , collapsedHeight :: String
  , component :: JSX
  , theme :: Foreign
  , timeout :: Foreign
  , style :: CSS
  , onEnter :: Foreign
  , onEntering :: Foreign
  , onEntered :: Foreign
  , onExit :: Foreign
  , onExiting :: Foreign
  , onExited :: Foreign
  , in :: Boolean
  , mountOnEnter :: Boolean
  , unmountOnExit :: Boolean
  , addEndListener :: Foreign
  , appear :: Boolean
  , enter :: Boolean
  , exit :: Boolean
  , classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , ref :: Foreign
  }

type CollapseProps_required =
  ( 
  )

type CollapseProps_optional =
  ( children :: JSX
  , collapsedHeight :: String
  , component :: JSX
  , theme :: Foreign
  , timeout :: Foreign
  , style :: CSS
  , onEnter :: Foreign
  , onEntering :: Foreign
  , onEntered :: Foreign
  , onExit :: Foreign
  , onExiting :: Foreign
  , onExited :: Foreign
  , in :: Boolean
  , mountOnEnter :: Boolean
  , unmountOnExit :: Boolean
  , addEndListener :: Foreign
  , appear :: Boolean
  , enter :: Boolean
  , exit :: Boolean
  , classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , ref :: Foreign
  )

collapse :: JSX
collapse = _Collapse
foreign import _Collapse :: JSX