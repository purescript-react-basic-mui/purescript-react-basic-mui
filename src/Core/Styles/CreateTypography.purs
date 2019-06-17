module React.Basic.MUI.Core.Styles.CreateTypography where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.DOM.Internal (CSS)
import React.Basic.MUI.Core.Styles.CreatePalette (Palette)

type ThemeStyle = Foreign

type FontStyle_required  optional =
  ( fontFamily :: Foreign
  , fontSize :: Number
  , fontWeightLight :: Foreign
  , fontWeightRegular :: Foreign
  , fontWeightMedium :: Foreign
  , fontWeightBold :: Foreign
  | optional )

type FontStyle_optional =
  ( 
  )

foreign import data FontStyle :: Type 



type FontStyleOptions_optional =
  ( htmlFontSize :: Number
  , allVariants :: CSS
  , fontFamily :: Foreign
  , fontSize :: Number
  , fontWeightLight :: Foreign
  , fontWeightRegular :: Foreign
  , fontWeightMedium :: Foreign
  , fontWeightBold :: Foreign
  )

foreign import data FontStyleOptions :: Type 



type TypographyStyle = Foreign

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

foreign import data TypographyStyleOptions :: Type 



type TypographyUtils_required  optional =
  ( pxToRem :: Foreign
  | optional )

type TypographyUtils_optional =
  ( 
  )

foreign import data TypographyUtils :: Type 



type Typography_required  optional =
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
  , fontWeightBold :: Foreign
  , pxToRem :: Foreign
  | optional )

type Typography_optional =
  ( 
  )

foreign import data Typography :: Type 



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
  , fontWeightBold :: Foreign
  )

foreign import data TypographyOptions :: Type 



createTypography :: Typography 
createTypography = _createTypography
foreign import _createTypography :: Typography 