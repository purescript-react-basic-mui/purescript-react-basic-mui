module Test.Main where

import Prelude

import Codegen.Main as Main
import Effect (Effect)

main :: Effect Unit
main = Main.main 
  