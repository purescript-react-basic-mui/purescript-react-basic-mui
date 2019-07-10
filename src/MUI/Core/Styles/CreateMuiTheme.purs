module MUI.Core.Styles.CreateMuiTheme where

import Prelude

import Data.Maybe (Maybe)
import Foreign (Foreign)
import MUI.Core (NumberToNumber)
import MUI.Core.Styles.CreateBreakpoints (BreakpointsOptions, Breakpoints)
import MUI.Core.Styles.CreateMixins (MixinsOptions, Mixins)
import MUI.Core.Styles.CreatePalette (PaletteOptions, Palette)
import MUI.Core.Styles.Shape (ShapeOptions, Shape)
import MUI.Core.Styles.Transitions (TransitionsOptions, Transitions)
import MUI.Core.Styles.Typography (TypographyOptions, Typography)
import MUI.Core.Styles.ZIndex (ZIndexOptions, ZIndex)
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
  , typography :: TypographyOptions
  , zIndex :: Maybe ZIndexOptions
  }

type Theme =
  { shape :: Shape
  , breakpoints :: Breakpoints
  , direction :: String 
  , mixins :: Mixins
  , overrides :: Foreign
  , palette :: Palette
  , props :: Foreign 
  , shadows :: Array String
  , spacing :: Number -> Number 
  , transitions :: Transitions
  , typography :: Typography
  , zIndex :: ZIndex
  }

createMuiTheme :: ThemeOptions -> Theme
createMuiTheme = write >>> _createMuiTheme

foreign import _createMuiTheme :: Foreign -> Theme