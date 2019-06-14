module React.Basic.MUI.Styles.CreateTypography where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.DOM.Internal (CSS)
import React.Basic.MUI.Styles.CreatePalette (Palette)

type ThemeStyle = Foreign

type FontStyle_required optional =
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

fontStyle
  :: ∀ attrs attrs_
   . Union attrs attrs_ (FontStyle_optional)
  => Record (FontStyle_required attrs)
  -> FontStyle
fontStyle = unsafeCoerce

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

fontStyleOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (FontStyleOptions_optional)
  => Record (attrs)
  -> FontStyleOptions
fontStyleOptions = unsafeCoerce

type TypographyStyle = Foreign

type TypographyStyleOptions_optional =
  ( color :: Foreign
  , fontFamily :: FontFamilyProperty 
  , fontSize :: FontSizeProperty TLength 
  , fontStyle :: FontStyleProperty 
  , fontWeight :: FontWeightProperty 
  , letterSpacing :: LetterSpacingProperty TLength 
  , lineHeight :: LineHeightProperty TLength 
  , textTransform :: TextTransformProperty 
  )

foreign import data TypographyStyleOptions :: Type 

typographyStyleOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TypographyStyleOptions_optional)
  => Record (attrs)
  -> TypographyStyleOptions
typographyStyleOptions = unsafeCoerce

type TypographyUtils_required optional =
  ( pxToRem :: Foreign
  | optional )

type TypographyUtils_optional =
  ( 
  )

foreign import data TypographyUtils :: Type 

typographyUtils
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TypographyUtils_optional)
  => Record (TypographyUtils_required attrs)
  -> TypographyUtils
typographyUtils = unsafeCoerce

type Typography_required optional =
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

typography
  :: ∀ attrs attrs_
   . Union attrs attrs_ (Typography_optional)
  => Record (Typography_required attrs)
  -> Typography
typography = unsafeCoerce

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

typographyOptions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TypographyOptions_optional)
  => Record (attrs)
  -> TypographyOptions
typographyOptions = unsafeCoerce

createTypography :: Typography 
createTypography = _createTypography
foreign import _createTypography :: Typography 