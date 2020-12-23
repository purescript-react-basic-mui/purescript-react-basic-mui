module MUI.Core.Styles.CreateMuiTheme where

import Prelude
import Foreign (Foreign, unsafeToForeign)
import MUI.Core.Styles.CreateBreakpoints (Breakpoints)
import MUI.Core.Styles.CreateMixins (MixinsOptions)
import MUI.Core.Styles.CreatePalette (PaletteOptions)
import MUI.Core.Styles.CreateTypography (TypographyOptions)
import MUI.Core.Styles.Shape (ShapeOptions)
import MUI.Core.Styles.Transitions (TransitionsOptions)
import MUI.Core.Styles.Types (ComponentsPropsPartial, Direction, OverridesPartial, Theme)
import MUI.Core.Styles.ZIndex (ZIndexOptions)
import MUI.System.Shadows (Shadows)
import Prim.Row (class Union)

-- | TODO: We should use this type as a value for `spacing` (`@material-ui/core/styles/createSpacing.d.ts`):
-- |
-- | export type SpacingOptions = number | ((factor: number) => string | number) | number[];
type ThemePartial
  = ( shape :: ShapeOptions
    , breakpoints :: Breakpoints
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
