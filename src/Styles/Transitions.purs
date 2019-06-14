module React.Basic.MUI.Styles.Transitions where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type Easing  =
  { easeInOut :: String
  , easeOut :: String
  , easeIn :: String
  , sharp :: String
  }

type Easing_required =
  ( easeInOut :: String
  , easeOut :: String
  , easeIn :: String
  , sharp :: String
  )

type Easing_optional =
  ( 
  )

easing :: Foreign
easing = _easing
foreign import _easing :: Foreign

type Duration  =
  { shortest :: Number
  , shorter :: Number
  , short :: Number
  , standard :: Number
  , complex :: Number
  , enteringScreen :: Number
  , leavingScreen :: Number
  }

type Duration_required =
  ( shortest :: Number
  , shorter :: Number
  , short :: Number
  , standard :: Number
  , complex :: Number
  , enteringScreen :: Number
  , leavingScreen :: Number
  )

type Duration_optional =
  ( 
  )

duration :: Foreign
duration = _duration
foreign import _duration :: Foreign

formatMs :: Foreign -> String
formatMs = _formatMs
foreign import _formatMs :: Foreign -> String

type Transitions  =
  { easing :: Foreign
  , duration :: Foreign
  , create :: Foreign
  , getAutoHeightDuration :: Foreign
  }

type Transitions_required =
  ( easing :: Foreign
  , duration :: Foreign
  , create :: Foreign
  , getAutoHeightDuration :: Foreign
  )

type Transitions_optional =
  ( 
  )

type TransitionsOptions  =
  { easing :: Foreign
  , duration :: Foreign
  , create :: Foreign
  , getAutoHeightDuration :: Foreign
  }

type TransitionsOptions_required =
  ( 
  )

type TransitionsOptions_optional =
  ( easing :: Foreign
  , duration :: Foreign
  , create :: Foreign
  , getAutoHeightDuration :: Foreign
  )

transitions :: Foreign
transitions = _transitions
foreign import _transitions :: Foreign