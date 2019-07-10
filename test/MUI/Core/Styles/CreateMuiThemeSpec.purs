module MUI.Core.Styles.CreateMuiThemeSpec where

import Prelude

import Data.Either (isRight)
import Data.Function.Uncurried (runFn2)
import Effect.Aff (Aff)
import Foreign (unsafeToForeign)
import MUI.Core.Styles.CreateMuiTheme (createMuiTheme, themeOptions)
import MUI.Core.Styles.Typography (TypographyStyle)
import Simple.JSON (class ReadForeign, E, read)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)


decode :: ∀ a. ReadForeign a => a -> E a
decode a = read $ unsafeToForeign a

isGood :: ∀ a. E a -> Aff Unit
isGood a = (isRight a) `shouldEqual` true

testTypographyStyle :: TypographyStyle -> Aff Unit
testTypographyStyle style = do
  isGood $ decode style.fontFamily
  isGood $ decode style.fontSize
  isGood $ decode style.fontWeight
  isGood $ decode style.letterSpacing
  isGood $ decode style.lineHeight
  isGood $ decode style.textTransform

spec :: Spec Unit
spec =
  describe "CretingMuiTheme" do
    it "should create a theme with proper Breakpoints" do
      let theme = createMuiTheme themeOptions
      isGood $ decode theme.breakpoints.keys
      isGood $ decode theme.breakpoints.values.xs
      isGood $ decode theme.breakpoints.values.sm
      isGood $ decode theme.breakpoints.values.md
      isGood $ decode theme.breakpoints.values.lg
      isGood $ decode theme.breakpoints.values.xl
      theme.breakpoints.up "sm" `shouldEqual` "@media (min-width:600px)"
      theme.breakpoints.down "sm" `shouldEqual` "@media (max-width:959.95px)"
      runFn2 theme.breakpoints.between "xs" "sm" `shouldEqual`  "@media (min-width:0px) and (max-width:959.95px)"
      theme.breakpoints.only "xs" `shouldEqual` "@media (min-width:0px) and (max-width:599.95px)"
      theme.breakpoints.width "xs" `shouldEqual` 0.0 

    it "should create a theme with a proper Palette" do
      let theme = createMuiTheme themeOptions
      isGood $ decode theme.palette.common
      isGood $ decode theme.palette.type
      isGood $ decode theme.palette.contrastThreshold
      isGood $ decode theme.palette.tonalOffset
      isGood $ decode theme.palette.primary
      isGood $ decode theme.palette.secondary
      isGood $ decode theme.palette.error
      isGood $ decode theme.palette.grey
      isGood $ decode theme.palette.text
      isGood $ decode theme.palette.divider
      isGood $ decode theme.palette.action
      isGood $ decode theme.palette.background
      (theme.palette.getContrastText "#AAA" == "rgba(0, 0, 0, 0.87)") `shouldEqual` true
      pure unit


    it "should create a theme with a proper Typography" do
      let theme = createMuiTheme themeOptions
      testTypographyStyle theme.typography.h1 
      testTypographyStyle theme.typography.h2
      testTypographyStyle theme.typography.h3
      testTypographyStyle theme.typography.h4
      testTypographyStyle theme.typography.h5
      testTypographyStyle theme.typography.h6
      testTypographyStyle theme.typography.subtitle1
      testTypographyStyle theme.typography.subtitle2
      testTypographyStyle theme.typography.body1
      testTypographyStyle theme.typography.body2
      testTypographyStyle theme.typography.caption
      testTypographyStyle theme.typography.button
      testTypographyStyle theme.typography.overline
      isGood $ decode theme.typography.fontFamily
      isGood $ decode theme.typography.fontWeightLight
      isGood $ decode theme.typography.fontWeightRegular
      isGood $ decode theme.typography.fontWeightMedium
      isGood $ decode theme.typography.fontWeightBold
      isGood $ decode theme.typography.letterSpacing
      isGood $ decode theme.typography.lineHeight
      isGood $ decode theme.typography.textTransform
      (theme.typography.pxToRem 12.0 == "0.75rem") `shouldEqual` true

    it "should create a theme with a proper Spacing" do
      let theme = createMuiTheme themeOptions
      testTypographyStyle theme.typography.h1 
      (theme.spacing 2.0 == 16.0) `shouldEqual` true


    it "should create a theme with a proper Shadows" do
      let theme = createMuiTheme themeOptions
      isGood $ decode theme.shadows