module React.Basic.MUI.Styles.ResponsiveFontSizes where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type ResponsiveFontSizesOptions  =
  { breakpoints :: Foreign
  , disableAlign :: Boolean
  , factor :: Number
  , variants :: Foreign
  }

type ResponsiveFontSizesOptions_required =
  ( 
  )

type ResponsiveFontSizesOptions_optional =
  ( breakpoints :: Foreign
  , disableAlign :: Boolean
  , factor :: Number
  , variants :: Foreign
  )

responsiveFontSizes :: Foreign -> Foreign
responsiveFontSizes = _responsiveFontSizes
foreign import _responsiveFontSizes :: Foreign -> Foreign