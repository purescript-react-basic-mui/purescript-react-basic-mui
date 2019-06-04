-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/styles/createMuiTheme.d.ts
module MaterialUI.Basic.Styles.CreateMuiTheme where 
import Data.Undefinable (Undefinable)
import Foreign (Foreign)
import React.Basic (ReactComponent)

import MaterialUI.Basic.Styles.CreateBreakpoints (Breakpoints)
import MaterialUI.Basic.Styles.CreateMixins (Mixins)
import MaterialUI.Basic.Styles.CreatePalette (Palette)
import MaterialUI.Basic.Styles.Shape (Shape)
import MaterialUI.Basic.Styles.CreateSpacing (Spacing)
import MaterialUI.Basic.Styles.Transitions (Transitions)
import MaterialUI.Basic.Styles.CreateTypography (Typography)
import MaterialUI.Basic.Styles.ZIndex (ZIndex)




foreign import _createMuiTheme :: forall a. ReactComponent a

type Theme  = {
    breakpoints :: Breakpoints
  , direction :: String
  , mixins :: Mixins
  , overrides  :: (Undefinable  Foreign)
  , palette :: Palette
  , props  :: (Undefinable  Foreign)
  , shadows :: Foreign
  , shape :: Shape
  , spacing :: Spacing
  , transitions :: Transitions
  , typography :: Typography
  , zIndex :: ZIndex
}

  
