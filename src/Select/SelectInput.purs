module React.Basic.MUI.Select.SelectInput where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type SelectInputProps  =
  { autoFocus :: Boolean
  , autoWidth :: Boolean
  , disabled :: Boolean
  , "IconComponent" :: JSX
  , inputRef :: Foreign
  , "MenuProps" :: Foreign
  , multiple :: Boolean
  , name :: String
  , native :: Boolean
  , onBlur :: EventHandler
  , onChange :: Foreign
  , onClose :: Foreign
  , onFocus :: EventHandler
  , onOpen :: Foreign
  , open :: Boolean
  , readOnly :: Boolean
  , renderValue :: Foreign
  , "SelectDisplayProps" :: Foreign
  , tabIndex :: Number
  , value :: Foreign
  , variant :: Foreign
  }

type SelectInputProps_required =
  ( autoWidth :: Boolean
  , multiple :: Boolean
  , native :: Boolean
  , value :: Foreign
  )

type SelectInputProps_optional =
  ( autoFocus :: Boolean
  , disabled :: Boolean
  , "IconComponent" :: JSX
  , inputRef :: Foreign
  , "MenuProps" :: Foreign
  , name :: String
  , onBlur :: EventHandler
  , onChange :: Foreign
  , onClose :: Foreign
  , onFocus :: EventHandler
  , onOpen :: Foreign
  , open :: Boolean
  , readOnly :: Boolean
  , renderValue :: Foreign
  , "SelectDisplayProps" :: Foreign
  , tabIndex :: Number
  , variant :: Foreign
  )

selectInput :: JSX
selectInput = _SelectInput
foreign import _SelectInput :: JSX