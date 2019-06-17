module React.Basic.MUI.Core.Styles.ResponsiveFontSizes where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Styles.CreateBreakpoints (Breakpoint)
import React.Basic.MUI.Core.Styles.CreateTypography (ThemeStyle)
import React.Basic.MUI.Core.Styles.CreateMuiTheme (Theme)

type ResponsiveFontSizesOptions_optional =
  ( breakpoints :: (Array Breakpoint )
  , disableAlign :: Boolean
  , factor :: Number
  , variants :: ThemeStyle 
  )

foreign import data ResponsiveFontSizesOptions :: Type 



responsiveFontSizes :: Theme 
responsiveFontSizes = _responsiveFontSizes
foreign import _responsiveFontSizes :: Theme 