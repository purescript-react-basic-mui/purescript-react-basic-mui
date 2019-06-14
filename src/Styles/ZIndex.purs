module React.Basic.MUI.Styles.ZIndex where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type ZIndex  =
  { mobileStepper :: Number
  , appBar :: Number
  , drawer :: Number
  , modal :: Number
  , snackbar :: Number
  , tooltip :: Number
  }

type ZIndex_required =
  ( mobileStepper :: Number
  , appBar :: Number
  , drawer :: Number
  , modal :: Number
  , snackbar :: Number
  , tooltip :: Number
  )

type ZIndex_optional =
  ( 
  )

zIndex :: Foreign
zIndex = _zIndex
foreign import _zIndex :: Foreign