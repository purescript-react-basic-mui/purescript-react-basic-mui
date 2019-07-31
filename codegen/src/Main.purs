module Main where

import Prelude

import Codegen ( codegen)
import Codegen.Core.ExpansionPanel as ExpansionPanel
import Codegen.Core.InputBase as InputBase
import Codegen.Model (Component)
import Effect (Effect)

components :: Array Component
components = 
  [ ExpansionPanel.component
  , InputBase.component
  ]

main :: Effect Unit
main = do
  let code = map codegen components >>= identity
  pure unit