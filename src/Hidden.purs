module React.Basic.MUI.Hidden where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type HiddenProps  =
  { implementation :: Foreign
  , initialWidth :: Foreign
  , lgDown :: Boolean
  , lgUp :: Boolean
  , mdDown :: Boolean
  , mdUp :: Boolean
  , only :: Foreign
  , smDown :: Boolean
  , smUp :: Boolean
  , xlDown :: Boolean
  , xlUp :: Boolean
  , xsDown :: Boolean
  , xsUp :: Boolean
  }

type HiddenProps_required =
  ( 
  )

type HiddenProps_optional =
  ( implementation :: Foreign
  , initialWidth :: Foreign
  , lgDown :: Boolean
  , lgUp :: Boolean
  , mdDown :: Boolean
  , mdUp :: Boolean
  , only :: Foreign
  , smDown :: Boolean
  , smUp :: Boolean
  , xlDown :: Boolean
  , xlUp :: Boolean
  , xsDown :: Boolean
  , xsUp :: Boolean
  )

hidden :: JSX
hidden = _Hidden
foreign import _Hidden :: JSX