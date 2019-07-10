module MUI.Core.Styles.CreatePalette where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import MUI.Core (Color, StringToString)
import MUI.Core.Colors.Common (CommonColors)
import Simple.JSON (write)

type PaletteColorOptions =
  { light :: Maybe String
  , main :: String
  , dark :: Maybe String
  , contrastText :: Maybe String
  }

paletteColorOptions :: String -> PaletteColorOptions
paletteColorOptions main = 
  { light : Nothing
  , main 
  , dark : Nothing
  , contrastText : Nothing
  }

type PaletteColor =
  { light :: String
  , main :: String
  , dark :: String
  , contrastText :: String
  }

type TypeBackground =
  { default :: String
  , paper :: String
  }

type TypeText = 
  { primary :: String
  , secondary :: String
  , disabled :: String
  , hint :: String
  }

type TypeAction =
  { active :: String
  , hover :: String
  , hoverOpacity :: Number
  , selected :: String
  , disabled :: String
  , disabledBackground :: String
  }

type PaletteOptions =
  { primary :: Maybe PaletteColorOptions
  , secondary :: Maybe PaletteColorOptions
  , error :: Maybe PaletteColorOptions
  , type :: Maybe String 
  , tonalOffset :: Maybe Number
  , contrastThreshold :: Maybe Number
  , common :: Maybe CommonColors
  , grey :: Maybe Color
  , text :: Maybe TypeText
  , divider :: Maybe String
  , action :: Maybe TypeAction
  , background :: Maybe TypeBackground
  , getContrastText :: Maybe StringToString
  }

paletteOptions :: PaletteOptions
paletteOptions =
  { primary : Nothing
  , secondary : Nothing
  , error : Nothing
  , type : Nothing
  , tonalOffset : Nothing
  , contrastThreshold : Nothing
  , common : Nothing
  , grey : Nothing
  , text : Nothing
  , divider : Nothing
  , action : Nothing
  , background : Nothing
  , getContrastText : Nothing
  }

type Palette = 
  { common :: CommonColors
  , type :: String
  , contrastThreshold :: Number
  , tonalOffset :: Number
  , primary :: PaletteColor
  , secondary :: PaletteColor
  , error :: PaletteColor
  , grey :: Color
  , text :: TypeText
  , divider :: String 
  , action :: TypeAction
  , background :: TypeBackground
  , getContrastText :: String -> String
  , augmentColor :: Foreign
  }

createPalette :: PaletteOptions -> Palette
createPalette = write >>> _createPalette

foreign import _createPalette :: Foreign -> Palette