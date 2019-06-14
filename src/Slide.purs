module React.Basic.MUI.Slide where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type SlideProps  =
  { direction :: Foreign
  , ref :: Foreign
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

type SlideProps_required =
  ( direction :: Foreign
  )

type SlideProps_optional =
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

slide :: JSX
slide = _Slide
foreign import _Slide :: JSX