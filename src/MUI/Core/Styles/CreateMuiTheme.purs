module MUI.Core.Styles.CreateMuiTheme where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import MUI.Core (NumberToNumber)
import MUI.Core.Styles.CreateBreakpoints (BreakpointsOptions, Breakpoints)
import MUI.Core.Styles.CreateMixins (MixinsOptions, Mixins)
import MUI.Core.Styles.CreatePalette (PaletteOptions, Palette)
import MUI.Core.Styles.Shape (ShapeOptions)
import MUI.Core.Styles.Transitions (TransitionsOptions)
import MUI.Core.Styles.Typography (TypographyOptions, Typography)
import MUI.Core.Styles.ZIndex (ZIndexOptions)
import Simple.JSON (write)

type ThemeOptions =
  { shape :: Maybe ShapeOptions
  , breakpoints :: Maybe BreakpointsOptions
  , direction :: Maybe String 
  , mixins :: Maybe MixinsOptions
  , overrides :: Maybe Foreign
  , palette :: Maybe PaletteOptions
  , props :: Maybe Foreign 
  , shadows :: Maybe (Array String)
  , spacing :: Maybe NumberToNumber
  , transitions :: Maybe TransitionsOptions
  , typography :: Maybe TypographyOptions
  , zIndex :: Maybe ZIndexOptions
  }

themeOptions :: ThemeOptions
themeOptions =
  { shape : Nothing
  , breakpoints : Nothing
  , direction : Nothing
  , mixins : Nothing
  , overrides : Nothing
  , palette : Nothing
  , props : Nothing
  , shadows : Nothing
  , spacing : Nothing
  , transitions : Nothing
  , typography : Nothing
  , zIndex : Nothing
  }

type Theme =
  { shape :: Foreign
  , breakpoints :: Breakpoints
  , direction :: Foreign
  , mixins :: Mixins
  , overrides :: Foreign
  , palette :: Palette
  , props :: Foreign 
  , shadows :: Array String
  , spacing :: Number -> Number 
  , transitions :: Foreign
  , typography :: Typography
  , zIndex :: Foreign
  }

createMuiTheme :: ThemeOptions -> Theme
createMuiTheme = write >>> _createMuiTheme

foreign import _createMuiTheme :: Foreign -> Theme