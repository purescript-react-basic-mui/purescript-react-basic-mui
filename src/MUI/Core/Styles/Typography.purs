module MUI.Core.Styles.Typography where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Foreign (Foreign, unsafeToForeign)
import MUI.Core (JSS)
import MUI.Core.Styles.CreatePalette (Palette)
import Simple.JSON (write)

type TypographyStyle =
  { fontFamily :: String
  , fontSize :: String
  , fontWeight :: Number
  , letterSpacing :: Foreign
  , lineHeight :: Foreign
  , textTransform :: Foreign
  } 

type TypographyOptions =
  { h1 :: Maybe JSS
  , h2 :: Maybe JSS
  , h3 :: Maybe JSS
  , h4 :: Maybe JSS
  , h5 :: Maybe JSS
  , h6 :: Maybe JSS
  , subtitle1 :: Maybe JSS
  , subtitle2 :: Maybe JSS
  , body1 :: Maybe JSS
  , body2 :: Maybe JSS
  , caption :: Maybe JSS
  , button :: Maybe JSS
  , overline :: Maybe JSS
  }

type Typography =
  { h1 :: TypographyStyle
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

createTypography :: Palette -> TypographyOptions -> Typography
createTypography palette options = runFn2 _createTypography (unsafeToForeign palette) (write options)

foreign import _createTypography :: Fn2 Foreign Foreign Typography