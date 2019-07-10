module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import MUI.Core.Styles.CreateMuiThemeSpec as CreateMuiThemeSpec
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do 
  CreateMuiThemeSpec.spec 
