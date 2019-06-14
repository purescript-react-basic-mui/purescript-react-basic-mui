module React.Basic.MUI.Fade where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type FadeProps  =
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

type FadeProps_required =
  ( 
  )

type FadeProps_optional =
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

fade :: JSX
fade = _Fade
foreign import _Fade :: JSX