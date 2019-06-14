module React.Basic.MUI.Portal where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type PortalProps  =
  { children :: Foreign
  , container :: Foreign
  , disablePortal :: Boolean
  , onRendered :: Foreign
  }

type PortalProps_required =
  ( children :: Foreign
  )

type PortalProps_optional =
  ( container :: Foreign
  , disablePortal :: Boolean
  , onRendered :: Foreign
  )