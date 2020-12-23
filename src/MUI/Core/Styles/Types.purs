module MUI.Core.Styles.Types where

import Data.Function.Uncurried (Fn4, runFn4)
import MUI.Core.Styles.CreateBreakpoints (Breakpoints)
import MUI.Core.Styles.CreateMixins (Mixins)
import MUI.Core.Styles.CreatePalette (Palette)
import MUI.Core.Styles.CreateTypography (Typography)
import MUI.Core.Styles.Shape (Shape)
import MUI.Core.Styles.Transitions (Transitions)
import MUI.Core.Styles.ZIndex (ZIndex)
import MUI.System.Shadows (Shadows)
import Unsafe.Coerce (unsafeCoerce)

foreign import data OverridesPartial :: Type

foreign import data ComponentsPropsPartial :: Type

-- | TODO:
-- | we should handle this (@material-ui/core/styles/createSpacing.d.ts)
-- | type value as an output value from `spacing`:
-- | export interface Spacing {
-- |   (): number;
-- |   (value: number): number;
-- |   (topBottom: SpacingArgument, rightLeft: SpacingArgument): string;
-- |   (top: SpacingArgument, rightLeft: SpacingArgument, bottom: SpacingArgument): string;
-- |   (
-- |     top: SpacingArgument,
-- |     right: SpacingArgument,
-- |     bottom: SpacingArgument,
-- |     left: SpacingArgument
-- |   ): string;
-- | }
type Theme
  = { shape :: Shape
    , breakpoints :: Breakpoints
    , direction :: Direction
    , mixins :: Mixins
    , overrides :: OverridesPartial
    , palette :: Palette
    , props :: ComponentsPropsPartial
    , shadows :: Shadows
    , spacing :: Number -> Number
    , transitions :: Transitions
    , typography :: Typography
    , zIndex :: ZIndex
    }

foreign import data SpacingParam ∷ Type

-- | padding: theme.spacing(1, 2, 3, 1), // '8px 16px 24px 8px'
-- | margin: theme.spacing(1, 'auto'), // '8px auto'
multiplier ∷ Number → SpacingParam
multiplier = unsafeCoerce

constant ∷ String → SpacingParam
constant = unsafeCoerce

spacing ∷ SpacingParam → SpacingParam → SpacingParam → SpacingParam → Theme → String
spacing p1 p2 p3 p4 theme = runFn4 spacing' p1 p2 p3 p4
  where
  spacing' ∷ Fn4 SpacingParam SpacingParam SpacingParam SpacingParam String
  spacing' = unsafeCoerce theme.spacing

foreign import data Direction :: Type

direction :: { ltr :: Direction, rtl ∷ Direction }
direction = { ltr: unsafeCoerce "ltr", rtl: unsafeCoerce "rtl" }
