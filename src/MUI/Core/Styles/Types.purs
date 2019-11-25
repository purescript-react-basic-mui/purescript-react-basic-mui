module MUI.Core.Styles.Types where

import MUI.Core.Styles.CreateBreakpoints (Breakpoints)
import MUI.Core.Styles.CreateMixins (Mixins)
import MUI.Core.Styles.CreatePalette (Palette)
import MUI.Core.Styles.Shape (Shape)
import MUI.Core.Styles.Transitions (Transitions)
import MUI.Core.Styles.CreateTypography (Typography)
import MUI.Core.Styles.ZIndex (ZIndex)
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
direction :: { ltr :: Direction, rtl ∷ Direction }
direction = { ltr: unsafeCoerce "ltr", rtl: unsafeCoerce "rtl" }

