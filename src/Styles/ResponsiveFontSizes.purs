module React.Basic.MUI.Styles.ResponsiveFontSizes where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Styles.CreateBreakpoints (Breakpoint)
import React.Basic.MUI.Styles.CreateTypography (ThemeStyle)
import React.Basic.MUI.Styles.CreateMuiTheme (Theme)

type ResponsiveFontSizesOptions_optional =
  ( breakpoints :: (Array Breakpoint )
  , disableAlign :: Boolean
  , factor :: Number
  , variants :: ThemeStyle 
  )

foreign import data ResponsiveFontSizesOptions :: Type 

responsiveFontSizesOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ResponsiveFontSizesOptions_optional)
  => Record (attrs)
  -> ResponsiveFontSizesOptions
responsiveFontSizesOptions = unsafeCoerce

responsiveFontSizes :: Theme 
responsiveFontSizes = _responsiveFontSizes
foreign import _responsiveFontSizes :: Theme 