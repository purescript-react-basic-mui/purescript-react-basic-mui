module Codegen.Main where

import Prelude

import Codegen.Read (DeclarationElements(..), interfaceByName, liftMaybe, showDeclarationElements, sourceFiles)
import Data.Array as Array
import Effect (Effect)
import Effect.Console (log)


main :: Effect Unit
main = do
  sources       <- sourceFiles
  cardProps     <- liftMaybe "Couldn't find CardProps" 
                      $ Array.head
                      $ Array.mapMaybe (\src -> interfaceByName src "CardProps") sources
  log $ showDeclarationElements (InterfaceDeclaration cardProps)

