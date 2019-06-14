module React.Basic.MUI.Styles.CreatePalette where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type TypeText  =
  { primary :: String
  , secondary :: String
  , disabled :: String
  , hint :: String
  }

type TypeText_required =
  ( primary :: String
  , secondary :: String
  , disabled :: String
  , hint :: String
  )

type TypeText_optional =
  ( 
  )

type TypeAction  =
  { active :: String
  , hover :: String
  , hoverOpacity :: Number
  , selected :: String
  , disabled :: String
  , disabledBackground :: String
  }

type TypeAction_required =
  ( active :: String
  , hover :: String
  , hoverOpacity :: Number
  , selected :: String
  , disabled :: String
  , disabledBackground :: String
  )

type TypeAction_optional =
  ( 
  )

type TypeBackground  =
  { default :: String
  , paper :: String
  }

type TypeBackground_required =
  ( default :: String
  , paper :: String
  )

type TypeBackground_optional =
  ( 
  )

type SimplePaletteColorOptions  =
  { light :: String
  , main :: String
  , dark :: String
  , contrastText :: String
  }

type SimplePaletteColorOptions_required =
  ( main :: String
  )

type SimplePaletteColorOptions_optional =
  ( light :: String
  , dark :: String
  , contrastText :: String
  )

type PaletteColor  =
  { light :: String
  , main :: String
  , dark :: String
  , contrastText :: String
  }

type PaletteColor_required =
  ( light :: String
  , main :: String
  , dark :: String
  , contrastText :: String
  )

type PaletteColor_optional =
  ( 
  )

type TypeObject  =
  { text :: Foreign
  , action :: Foreign
  , divider :: Foreign
  , background :: Foreign
  }

type TypeObject_required =
  ( text :: Foreign
  , action :: Foreign
  , divider :: Foreign
  , background :: Foreign
  )

type TypeObject_optional =
  ( 
  )

light :: Foreign
light = _light
foreign import _light :: Foreign

dark :: Foreign
dark = _dark
foreign import _dark :: Foreign

type Palette  =
  { common :: Foreign
  , type :: Foreign
  , contrastThreshold :: Number
  , tonalOffset :: Number
  , primary :: Foreign
  , secondary :: Foreign
  , error :: Foreign
  , grey :: Foreign
  , text :: Foreign
  , divider :: Foreign
  , action :: Foreign
  , background :: Foreign
  , getContrastText :: Foreign
  , augmentColor :: Foreign
  }

type Palette_required =
  ( common :: Foreign
  , type :: Foreign
  , contrastThreshold :: Number
  , tonalOffset :: Number
  , primary :: Foreign
  , secondary :: Foreign
  , error :: Foreign
  , grey :: Foreign
  , text :: Foreign
  , divider :: Foreign
  , action :: Foreign
  , background :: Foreign
  , getContrastText :: Foreign
  , augmentColor :: Foreign
  )

type Palette_optional =
  ( 
  )

type PaletteOptions  =
  { primary :: Foreign
  , secondary :: Foreign
  , error :: Foreign
  , type :: Foreign
  , tonalOffset :: Number
  , contrastThreshold :: Number
  , common :: Foreign
  , grey :: Foreign
  , text :: Foreign
  , divider :: String
  , action :: Foreign
  , background :: Foreign
  , getContrastText :: Foreign
  }

type PaletteOptions_required =
  ( 
  )

type PaletteOptions_optional =
  ( primary :: Foreign
  , secondary :: Foreign
  , error :: Foreign
  , type :: Foreign
  , tonalOffset :: Number
  , contrastThreshold :: Number
  , common :: Foreign
  , grey :: Foreign
  , text :: Foreign
  , divider :: String
  , action :: Foreign
  , background :: Foreign
  , getContrastText :: Foreign
  )

createPalette :: Foreign -> Foreign
createPalette = _createPalette
foreign import _createPalette :: Foreign -> Foreign