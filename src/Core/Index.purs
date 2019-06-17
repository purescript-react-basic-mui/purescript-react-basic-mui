module React.Basic.MUI.Core.Index where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.DOM.Internal (CSS)

type StandardProps c classkey removals acceptsref = Foreign

type PaletteType = Foreign

type Color_required  optional =
  ( "50" :: String
  , "100" :: String
  , "200" :: String
  , "300" :: String
  , "400" :: String
  , "500" :: String
  , "600" :: String
  , "700" :: String
  , "800" :: String
  , "900" :: String
  , "A100" :: String
  , "A200" :: String
  , "A400" :: String
  , "A700" :: String
  | optional )

type Color_optional =
  ( 
  )

foreign import data Color :: Type 

