module React.Basic.MUI.Core.Styles.CreateMuiTheme where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Styles.Shape (Shape, ShapeOptions)
import React.Basic.MUI.Core.Styles.CreateBreakpoints (Breakpoints, BreakpointsOptions)
import React.Basic.MUI.Core.Styles.CreateMixins (Mixins, MixinsOptions)
import React.Basic.MUI.Core.Styles.Overrides (Overrides)
import React.Basic.MUI.Core.Styles.CreatePalette (Palette, PaletteOptions)
import React.Basic.MUI.Core.Styles.Shadows (Shadows)
import React.Basic.MUI.Core.Styles.CreateSpacing (Spacing, SpacingOptions)
import React.Basic.MUI.Core.Styles.Transitions (Transitions, TransitionsOptions)
import React.Basic.MUI.Core.Styles.CreateTypography (TypographyOptions)
import React.Basic.MUI.Core.Styles.ZIndex (ZIndex, ZIndexOptions)
import React.Basic.MUI.Core.Typography (Typography)

type Direction = Foreign

type ThemeOptions_optional =
  ( shape :: ShapeOptions 
  , breakpoints :: BreakpointsOptions 
  , direction :: Direction 
  , mixins :: MixinsOptions 
  , overrides :: Overrides 
  , palette :: PaletteOptions 
  , props :: Foreign
  , shadows :: Shadows 
  , spacing :: SpacingOptions 
  , transitions :: TransitionsOptions 
  , typography :: Foreign
  , zIndex :: ZIndexOptions 
  )

foreign import data ThemeOptions :: Type 



type Theme_required  optional =
  ( shape :: Shape 
  , breakpoints :: Breakpoints 
  , direction :: Direction 
  , mixins :: Mixins 
  , palette :: Palette 
  , shadows :: Shadows 
  , spacing :: Spacing 
  , transitions :: Transitions 
  , typography :: Typography 
  , zIndex :: ZIndex 
  | optional )

type Theme_optional =
  ( overrides :: Overrides 
  , props :: Foreign
  )

foreign import data Theme :: Type 



createMuiTheme :: Theme 
createMuiTheme = _createMuiTheme
foreign import _createMuiTheme :: Theme 