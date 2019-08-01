module Main where

import Prelude

import Codegen (codegen, write)
import Codegen.Core.ExpansionPanel as ExpansionPanel
import Codegen.Core.InputBase as InputBase
import Codegen.Model (Component)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (launchAff_)
import MUI.Core.Grid as Grid

components :: Array Component
components = 
  [ ExpansionPanel.component
  , InputBase.component
  , Grid.component
  ]

main :: Effect Unit
main = launchAff_ do
  let code = join $ map codegen components
  traverse_ (write "../src") code