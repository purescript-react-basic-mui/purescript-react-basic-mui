module React.Basic.MUI.RootRef where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type RootRefProps  t =
  { rootRef :: Foreign
  }

type RootRefProps_required t =
  ( 
  )

type RootRefProps_optional t =
  ( rootRef :: Foreign
  )

rootRef :: JSX
rootRef = _RootRef
foreign import _RootRef :: JSX