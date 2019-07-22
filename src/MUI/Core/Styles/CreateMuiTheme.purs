module MUI.Core.Styles.CreateMuiTheme where

import Prelude

import Foreign (Foreign, unsafeToForeign)
import MUI.Core.Styles.CreateBreakpoints (BreakpointsOptions, Breakpoints)
import MUI.Core.Styles.CreateMixins (MixinsOptions, Mixins)
import MUI.Core.Styles.CreatePalette (PaletteOptions, Palette)
import MUI.Core.Styles.Overrides (OverridesPartial)
import MUI.Core.Styles.Props (ComponentsPropsPartial)
import MUI.Core.Styles.Shape (ShapeOptions, Shape)
import MUI.Core.Styles.Transitions (TransitionsOptions, Transitions)
import MUI.Core.Styles.CreateTypography (Typography, TypographyOptions)
import MUI.Core.Styles.ZIndex (ZIndex, ZIndexOptions)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type ThemePartial =
  ( shape :: ShapeOptions
  , breakpoints :: BreakpointsOptions
  , direction :: DirectionProp
  , mixins :: MixinsOptions
  , overrides :: OverridesPartial
  , palette :: PaletteOptions
  , props :: ComponentsPropsPartial 
  , shadows :: Array String
  , spacing :: Number -> Number 
  , transitions :: TransitionsOptions
  , typography :: TypographyOptions
  , zIndex :: ZIndexOptions
  )

type Theme = 
  { shape :: Shape
  , breakpoints :: Breakpoints
  , direction :: DirectionProp
  , mixins :: Mixins
  , overrides :: OverridesPartial
  , palette :: Palette
  , props :: ComponentsPropsPartial
  , shadows :: Array String
  , spacing :: Number -> Number
  , transitions :: Transitions
  , typography :: Typography
  , zIndex :: ZIndex
  }

foreign import data DirectionProp :: Type
data Direction = LTR | RTL
direction :: Direction -> DirectionProp
direction LTR = unsafeCoerce "ltr"
direction RTL = unsafeCoerce "rtl"

createMuiTheme :: ∀ options options_
  . Union options options_ ThemePartial
  => Record options 
  -> Theme 
createMuiTheme = _createMuiTheme <<< unsafeToForeign

foreign import _createMuiTheme :: Foreign -> Theme