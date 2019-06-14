module React.Basic.MUI.Styles.ColorManipulator where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type ColorObject  =
  { type :: Foreign
  , values :: Foreign
  }

type ColorObject_required =
  ( type :: Foreign
  , values :: Foreign
  )

type ColorObject_optional =
  ( 
  )

recomposeColor :: Foreign -> String
recomposeColor = _recomposeColor
foreign import _recomposeColor :: Foreign -> String

convertHexToRGB :: Foreign -> String
convertHexToRGB = _convertHexToRGB
foreign import _convertHexToRGB :: Foreign -> String

rgbToHex :: Foreign -> String
rgbToHex = _rgbToHex
foreign import _rgbToHex :: Foreign -> String

decomposeColor :: Foreign -> Foreign
decomposeColor = _decomposeColor
foreign import _decomposeColor :: Foreign -> Foreign

getContrastRatio :: Foreign -> Number
getContrastRatio = _getContrastRatio
foreign import _getContrastRatio :: Foreign -> Number

getLuminance :: Foreign -> Number
getLuminance = _getLuminance
foreign import _getLuminance :: Foreign -> Number

emphasize :: Foreign -> String
emphasize = _emphasize
foreign import _emphasize :: Foreign -> String

fade :: Foreign -> String
fade = _fade
foreign import _fade :: Foreign -> String

darken :: Foreign -> String
darken = _darken
foreign import _darken :: Foreign -> String

lighten :: Foreign -> String
lighten = _lighten
foreign import _lighten :: Foreign -> String