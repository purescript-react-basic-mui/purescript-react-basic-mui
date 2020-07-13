module MUI.Core.Styles.CreateTypography where

import Data.Function.Uncurried (Fn2, runFn2)
import Foreign (Foreign, unsafeToForeign)
import MUI.Core (JSS)
import MUI.Core.Styles.CreatePalette (Palette)
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)

type TypographyStyle
  = { fontFamily :: String
    , fontSize :: String
    , fontWeight :: Number
    , letterSpacing :: Foreign
    , lineHeight :: Foreign
    , textTransform :: Foreign
    }

type TypographyPartial
  = ( h1 :: JSS
    , h2 :: JSS
    , h3 :: JSS
    , h4 :: JSS
    , h5 :: JSS
    , h6 :: JSS
    , subtitle1 :: JSS
    , subtitle2 :: JSS
    , body1 :: JSS
    , body2 :: JSS
    , caption :: JSS
    , button :: JSS
    , overline :: JSS
    )

foreign import data TypographyOptions :: Type

typographyOptions ::
  ∀ options options_.
  Union options options_ TypographyPartial =>
  Record options ->
  TypographyOptions
typographyOptions = unsafeCoerce

type Typography
  = { h1 :: TypographyStyle
    , h2 :: TypographyStyle
    , h3 :: TypographyStyle
    , h4 :: TypographyStyle
    , h5 :: TypographyStyle
    , h6 :: TypographyStyle
    , subtitle1 :: TypographyStyle
    , subtitle2 :: TypographyStyle
    , body1 :: TypographyStyle
    , body2 :: TypographyStyle
    , caption :: TypographyStyle
    , button :: TypographyStyle
    , overline :: TypographyStyle
    , fontFamily :: String
    , fontSize :: Number
    , fontWeightLight :: Number
    , fontWeightRegular :: Number
    , fontWeightMedium :: Number
    , fontWeightBold :: Number
    , letterSpacing :: Foreign
    , lineHeight :: Foreign
    , textTransform :: Foreign
    , pxToRem :: Number -> String
    }

createTypography ::
  ∀ options options_.
  Union options options_ TypographyPartial =>
  Palette ->
  Record options ->
  Typography
createTypography palette options = runFn2 _createTypography (unsafeToForeign palette) (unsafeToForeign options)

foreign import _createTypography :: Fn2 Foreign Foreign Typography
