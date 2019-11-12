module MUI.Core.Styles.Types where

import Foreign (Foreign, unsafeToForeign)
import MUI.Core.Styles.CreateBreakpoints (BreakpointsOptions, Breakpoints)
import MUI.Core.Styles.CreateMixins (MixinsOptions, Mixins)
import MUI.Core.Styles.CreatePalette (PaletteOptions, Palette)
import MUI.Core.Styles.Shape (ShapeOptions, Shape)
import MUI.Core.Styles.Transitions (TransitionsOptions, Transitions)
import MUI.Core.Styles.CreateTypography (Typography, TypographyOptions)
import MUI.Core.Styles.ZIndex (ZIndex, ZIndexOptions)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

foreign import data OverridesPartial :: Type

foreign import data ComponentsPropsPartial :: Type

type Theme =
  { shape :: Shape
  , breakpoints :: Breakpoints
  , direction :: Direction
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

foreign import data Direction :: Type
-- data Direction = LTR | RTL
direction :: { ltr ∷ Direction, rtl ∷ Direction }
direction = { ltr: unsafeCoerce "ltr", rtl: unsafeCoerce "rtl" }

