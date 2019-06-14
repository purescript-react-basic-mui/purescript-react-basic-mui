module React.Basic.MUI.Styles.CreateMuiTheme where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Styles.Shape (Shape, ShapeOptions)
import React.Basic.MUI.Styles.CreateBreakpoints (Breakpoints, BreakpointsOptions)
import React.Basic.MUI.Styles.CreateMixins (Mixins, MixinsOptions)
import React.Basic.MUI.Styles.Overrides (Overrides)
import React.Basic.MUI.Styles.CreatePalette (Palette, PaletteOptions)
import React.Basic (element, ReactComponent, ReactComponent)
import React.Basic.MUI.Styles.Shadows (Shadows)
import React.Basic.MUI.Styles.CreateSpacing (Spacing, SpacingOptions)
import React.Basic.MUI.Styles.Transitions (Transitions, TransitionsOptions)
import React.Basic.MUI.Styles.CreateTypography (TypographyOptions)
import React.Basic.MUI.Styles.ZIndex (ZIndex, ZIndexOptions)
import React.Basic.MUI.Typography (Typography)

type Direction = Foreign

type ThemeOptions_optional =
  ( shape :: ShapeOptions 
  , breakpoints :: BreakpointsOptions 
  , direction :: Direction 
  , mixins :: MixinsOptions 
  , overrides :: Overrides 
  , palette :: PaletteOptions 
  , props :: ReactComponent
  , shadows :: Shadows 
  , spacing :: SpacingOptions 
  , transitions :: TransitionsOptions 
  , typography :: Foreign
  , zIndex :: ZIndexOptions 
  )

foreign import data ThemeOptions :: Type 

themeOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ThemeOptions_optional)
  => Record (attrs)
  -> ThemeOptions
themeOptions = unsafeCoerce

type Theme_required optional =
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
  , props :: ReactComponent
  )

foreign import data Theme :: Type 

theme
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Theme_optional)
  => Record (Theme_required attrs)
  -> Theme
theme = unsafeCoerce

createMuiTheme :: Theme 
createMuiTheme = _createMuiTheme
foreign import _createMuiTheme :: Theme 