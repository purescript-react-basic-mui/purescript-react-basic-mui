module React.Basic.MUI.Zoom where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type ZoomProps  =
  { ref :: Foreign
  , theme :: Foreign
  , style :: CSS
  , appear :: Boolean
  , enter :: Boolean
  , exit :: Boolean
  , onEnter :: Foreign
  , onEntering :: Foreign
  , onEntered :: Foreign
  , onExit :: Foreign
  , onExiting :: Foreign
  , onExited :: Foreign
  , in :: Boolean
  , mountOnEnter :: Boolean
  , unmountOnExit :: Boolean
  , timeout :: Foreign
  , addEndListener :: Foreign
  }

type ZoomProps_required =
  ( 
  )

type ZoomProps_optional =
  ( ref :: Foreign
  , theme :: Foreign
  , style :: CSS
  , appear :: Boolean
  , enter :: Boolean
  , exit :: Boolean
  , onEnter :: Foreign
  , onEntering :: Foreign
  , onEntered :: Foreign
  , onExit :: Foreign
  , onExiting :: Foreign
  , onExited :: Foreign
  , in :: Boolean
  , mountOnEnter :: Boolean
  , unmountOnExit :: Boolean
  , timeout :: Foreign
  , addEndListener :: Foreign
  )

zoom :: JSX
zoom = _Zoom
foreign import _Zoom :: JSX