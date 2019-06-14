module React.Basic.MUI.NoSsr where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type NoSsrProps  =
  { children :: JSX
  , defer :: Boolean
  , fallback :: JSX
  }

type NoSsrProps_required =
  ( children :: JSX
  )

type NoSsrProps_optional =
  ( defer :: Boolean
  , fallback :: JSX
  )

noSsr :: JSX
noSsr = _NoSsr
foreign import _NoSsr :: JSX