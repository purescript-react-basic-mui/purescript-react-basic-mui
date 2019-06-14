module React.Basic.MUI.WithWidth where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type WithWidthOptions  =
  { withTheme :: Boolean
  , noSSR :: Boolean
  , initialWidth :: Foreign
  , resizeInterval :: Number
  }

type WithWidthOptions_required =
  ( 
  )

type WithWidthOptions_optional =
  ( withTheme :: Boolean
  , noSSR :: Boolean
  , initialWidth :: Foreign
  , resizeInterval :: Number
  )

type WithWidth  =
  { width :: Foreign
  }

type WithWidth_required =
  ( width :: Foreign
  )

type WithWidth_optional =
  ( 
  )

type WithWidthProps  =
  { innerRef :: Foreign
  , width :: Foreign
  }

type WithWidthProps_required =
  ( 
  )

type WithWidthProps_optional =
  ( innerRef :: Foreign
  , width :: Foreign
  )

isWidthDown :: Foreign -> Boolean
isWidthDown = _isWidthDown
foreign import _isWidthDown :: Foreign -> Boolean

isWidthUp :: Foreign -> Boolean
isWidthUp = _isWidthUp
foreign import _isWidthUp :: Foreign -> Boolean

withWidth :: Foreign -> Foreign
withWidth = _withWidth
foreign import _withWidth :: Foreign -> Foreign