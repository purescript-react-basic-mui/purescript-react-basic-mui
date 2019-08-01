module Main where

import Prelude

import Codegen (codegen, write)
import Codegen.Core.AppBar as AppBar
import Codegen.Core.Avatar as Avatar
import Codegen.Core.Backdrop as Backdrop
import Codegen.Core.Badge as Badge
import Codegen.Core.BottomNavigation as BottomNavigation
import Codegen.Core.ExpansionPanel as ExpansionPanel
import Codegen.Core.Grid as Grid
import Codegen.Core.InputBase as InputBase
import Codegen.Model (Component)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)

components :: Array Component
components = 
  [ AppBar.component
  , Avatar.component
  , Backdrop.component
  , Badge.component
  , BottomNavigation.component
  , ExpansionPanel.component
  , InputBase.component
  , Grid.component
  ]

main :: Effect Unit
main = launchAff_ do
  let code = join $ map codegen components
  traverse_ (write "../src") code