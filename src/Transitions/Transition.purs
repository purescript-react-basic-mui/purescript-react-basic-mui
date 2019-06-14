module React.Basic.MUI.Transitions.Transition where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type TransitionProps  =
  { style :: CSS
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

type TransitionProps_required =
  ( 
  )

type TransitionProps_optional =
  ( style :: CSS
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