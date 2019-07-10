module MUI.Core.Styles.Typography where

import Data.Function.Uncurried (Fn2, runFn2)
import Data.Maybe (Maybe)
import Foreign (Foreign, unsafeToForeign)
import MUI.Core (JSS)
import MUI.Core.Styles.CreatePalette (Palette)
import Simple.JSON (write)

type FontStyle =
  { fontFamily :: String
  , fontSize :: String
  , fontWeightLight :: String
  , fontWeightRegular :: String
  , fontWeightMedium :: String
  , fontWeightBold :: String
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
  { h1 :: FontStyle
  , h2 :: FontStyle
  , h3 :: FontStyle
  , h4 :: FontStyle
  , h5 :: FontStyle
  , h6 :: FontStyle
  , subtitle1 :: FontStyle
  , subtitle2 :: FontStyle
  , body1 :: FontStyle
  , body2 :: FontStyle
  , caption :: FontStyle
  , button :: FontStyle
  , overline :: FontStyle
  , fontFamily :: String
  , fontSize :: String
  , fontWeightLight :: String
  , fontWeightRegular :: String
  , fontWeightMedium :: String
  , fontWeightBold :: String
  , letterSpacing :: Foreign
  , lineHeight :: Foreign
  , textTransform :: Foreign
  , pxToRem :: Number -> String
  }

createTypography :: Palette -> TypographyOptions -> Typography
createTypography palette options = runFn2 _createTypography (unsafeToForeign palette) (write options)

foreign import _createTypography :: Fn2 Foreign Foreign Typography