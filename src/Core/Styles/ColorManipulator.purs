module React.Basic.MUI.Core.Styles.ColorManipulator where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)




type ColorFormat = Foreign

type ColorObject_required  optional =
  ( type :: Foreign
  , values :: Foreign
  | optional )

type ColorObject_optional =
  ( 
  )

foreign import data ColorObject :: Type 



hexToRgb :: String
hexToRgb = _hexToRgb
foreign import _hexToRgb :: String

rgbToHex :: String
rgbToHex = _rgbToHex
foreign import _rgbToHex :: String

hslToRgb :: String
hslToRgb = _hslToRgb
foreign import _hslToRgb :: String

decomposeColor :: Foreign
decomposeColor = _decomposeColor
foreign import _decomposeColor :: Foreign

recomposeColor :: String
recomposeColor = _recomposeColor
foreign import _recomposeColor :: String

getContrastRatio :: Number
getContrastRatio = _getContrastRatio
foreign import _getContrastRatio :: Number

getLuminance :: Number
getLuminance = _getLuminance
foreign import _getLuminance :: Number

emphasize :: String
emphasize = _emphasize
foreign import _emphasize :: String

fade :: String
fade = _fade
foreign import _fade :: String

darken :: String
darken = _darken
foreign import _darken :: String

lighten :: String
lighten = _lighten
foreign import _lighten :: String