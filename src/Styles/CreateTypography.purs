module React.Basic.MUI.Styles.CreateTypography where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type FontStyle  =
  { fontFamily :: Foreign
  , fontSize :: Number
  , fontWeightLight :: Foreign
  , fontWeightRegular :: Foreign
  , fontWeightMedium :: Foreign
  }

type FontStyle_required =
  ( fontFamily :: Foreign
  , fontSize :: Number
  , fontWeightLight :: Foreign
  , fontWeightRegular :: Foreign
  , fontWeightMedium :: Foreign
  )

type FontStyle_optional =
  ( 
  )

type FontStyleOptions  =
  { htmlFontSize :: Number
  , allVariants :: CSS
  , fontFamily :: Foreign
  , fontSize :: Number
  , fontWeightLight :: Foreign
  , fontWeightRegular :: Foreign
  , fontWeightMedium :: Foreign
  }

type FontStyleOptions_required =
  ( 
  )

type FontStyleOptions_optional =
  ( htmlFontSize :: Number
  , allVariants :: CSS
  , fontFamily :: Foreign
  , fontSize :: Number
  , fontWeightLight :: Foreign
  , fontWeightRegular :: Foreign
  , fontWeightMedium :: Foreign
  )

type TypographyStyleOptions  =
  { color :: Foreign
  , fontFamily :: Foreign
  , fontSize :: Foreign
  , fontStyle :: Foreign
  , fontWeight :: Foreign
  , letterSpacing :: Foreign
  , lineHeight :: Foreign
  , textTransform :: Foreign
  }

type TypographyStyleOptions_required =
  ( 
  )

type TypographyStyleOptions_optional =
  ( color :: Foreign
  , fontFamily :: Foreign
  , fontSize :: Foreign
  , fontStyle :: Foreign
  , fontWeight :: Foreign
  , letterSpacing :: Foreign
  , lineHeight :: Foreign
  , textTransform :: Foreign
  )

type TypographyUtils  =
  { pxToRem :: Foreign
  }

type TypographyUtils_required =
  ( pxToRem :: Foreign
  )

type TypographyUtils_optional =
  ( 
  )

type Typography  =
  { button :: Foreign
  , caption :: Foreign
  , h1 :: Foreign
  , h2 :: Foreign
  , h3 :: Foreign
  , h4 :: Foreign
  , h5 :: Foreign
  , h6 :: Foreign
  , overline :: Foreign
  , subtitle1 :: Foreign
  , subtitle2 :: Foreign
  , body1 :: Foreign
  , body2 :: Foreign
  , fontFamily :: Foreign
  , fontSize :: Number
  , fontWeightLight :: Foreign
  , fontWeightRegular :: Foreign
  , fontWeightMedium :: Foreign
  , pxToRem :: Foreign
  }

type Typography_required =
  ( button :: Foreign
  , caption :: Foreign
  , h1 :: Foreign
  , h2 :: Foreign
  , h3 :: Foreign
  , h4 :: Foreign
  , h5 :: Foreign
  , h6 :: Foreign
  , overline :: Foreign
  , subtitle1 :: Foreign
  , subtitle2 :: Foreign
  , body1 :: Foreign
  , body2 :: Foreign
  , fontFamily :: Foreign
  , fontSize :: Number
  , fontWeightLight :: Foreign
  , fontWeightRegular :: Foreign
  , fontWeightMedium :: Foreign
  , pxToRem :: Foreign
  )

type Typography_optional =
  ( 
  )

type TypographyOptions  =
  { button :: Foreign
  , caption :: Foreign
  , h1 :: Foreign
  , h2 :: Foreign
  , h3 :: Foreign
  , h4 :: Foreign
  , h5 :: Foreign
  , h6 :: Foreign
  , overline :: Foreign
  , subtitle1 :: Foreign
  , subtitle2 :: Foreign
  , body1 :: Foreign
  , body2 :: Foreign
  , htmlFontSize :: Number
  , allVariants :: CSS
  , fontFamily :: Foreign
  , fontSize :: Number
  , fontWeightLight :: Foreign
  , fontWeightRegular :: Foreign
  , fontWeightMedium :: Foreign
  }

type TypographyOptions_required =
  ( 
  )

type TypographyOptions_optional =
  ( button :: Foreign
  , caption :: Foreign
  , h1 :: Foreign
  , h2 :: Foreign
  , h3 :: Foreign
  , h4 :: Foreign
  , h5 :: Foreign
  , h6 :: Foreign
  , overline :: Foreign
  , subtitle1 :: Foreign
  , subtitle2 :: Foreign
  , body1 :: Foreign
  , body2 :: Foreign
  , htmlFontSize :: Number
  , allVariants :: CSS
  , fontFamily :: Foreign
  , fontSize :: Number
  , fontWeightLight :: Foreign
  , fontWeightRegular :: Foreign
  , fontWeightMedium :: Foreign
  )

createTypography :: Foreign -> Foreign
createTypography = _createTypography
foreign import _createTypography :: Foreign -> Foreign