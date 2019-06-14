module React.Basic.MUI.Grow where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type GrowProps  =
  { ref :: Foreign
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
  }

type GrowProps_required =
  ( 
  )

type GrowProps_optional =
  ( ref :: Foreign
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
  )

grow :: JSX
grow = _Grow
foreign import _Grow :: JSX