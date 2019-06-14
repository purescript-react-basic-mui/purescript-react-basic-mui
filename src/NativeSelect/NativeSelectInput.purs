module React.Basic.MUI.NativeSelect.NativeSelectInput where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type NativeSelectInputProps  =
  { disabled :: Boolean
  , "IconComponent" :: JSX
  , inputRef :: Foreign
  , name :: String
  , onChange :: Foreign
  , value :: Foreign
  , variant :: Foreign
  }

type NativeSelectInputProps_required =
  ( 
  )

type NativeSelectInputProps_optional =
  ( disabled :: Boolean
  , "IconComponent" :: JSX
  , inputRef :: Foreign
  , name :: String
  , onChange :: Foreign
  , value :: Foreign
  , variant :: Foreign
  )

nativeSelectInput :: JSX
nativeSelectInput = _NativeSelectInput
foreign import _NativeSelectInput :: JSX