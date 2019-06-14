module React.Basic.MUI.Colors.Common where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type CommonColors  =
  { black :: String
  , white :: String
  }

type CommonColors_required =
  ( black :: String
  , white :: String
  )

type CommonColors_optional =
  ( 
  )

common :: Foreign
common = _common
foreign import _common :: Foreign