module MUI.Core.Styles.CreateMuiTheme where

import Prelude

import Foreign (Foreign, unsafeToForeign)
import MUI.Core.Styles.CreateBreakpoints (BreakpointsOptions, Breakpoints)
import MUI.Core.Styles.CreateMixins (MixinsOptions, Mixins)
import MUI.Core.Styles.CreatePalette (PaletteOptions, Palette)
import MUI.Core.Styles.CreateTypography (Typography, TypographyOptions)
import MUI.Core.Styles.Shape (ShapeOptions, Shape)
import MUI.Core.Styles.Transitions (TransitionsOptions, Transitions)
import MUI.Core.Styles.Types (ComponentsPropsPartial, Direction, OverridesPartial, Theme)
import MUI.Core.Styles.ZIndex (ZIndex, ZIndexOptions)
import MUI.System.Shadows (Shadows)
import Prim.Row (class Union)

type ThemePartial
  = ( shape :: ShapeOptions
    , breakpoints :: BreakpointsOptions
    , direction :: Direction
    , mixins :: MixinsOptions
    , overrides :: OverridesPartial
    , palette :: PaletteOptions
    , props :: ComponentsPropsPartial
    , shadows :: Shadows
    , spacing :: Number -> Number
    , transitions :: TransitionsOptions
    , typography :: TypographyOptions
    , zIndex :: ZIndexOptions
    )

createMuiTheme ::
  ∀ options options_.
  Union options options_ ThemePartial =>
  Record options ->
  Theme
createMuiTheme = _createMuiTheme <<< unsafeToForeign

foreign import _createMuiTheme :: Foreign -> Theme
