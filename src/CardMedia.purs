module React.Basic.MUI.CardMedia where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type CardMediaTypeMap  p d =
  { props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  }

type CardMediaTypeMap_required p d =
  ( props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  )

type CardMediaTypeMap_optional p d =
  ( 
  )

cardMedia :: Foreign
cardMedia = _CardMedia
foreign import _CardMedia :: Foreign