module React.Basic.MUI.Styles.Shape where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type Shape  =
  { borderRadius :: Number
  }

type Shape_required =
  ( borderRadius :: Number
  )

type Shape_optional =
  ( 
  )

shape :: Foreign
shape = _shape
foreign import _shape :: Foreign