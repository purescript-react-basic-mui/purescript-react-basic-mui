module Codegen.Main where

import Prelude

import Codegen.Read (DeclarationElements(..), interfaceByName, liftEither, liftMaybe, showDeclarationElements, sourceFiles)
import Data.Array as Array
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Effect (Effect)
import Effect.Console (log)


main :: Effect Unit
main = do
  regex       <- liftEither $ Regex.regex ".*material-ui.*" RegexFlags.noFlags
  sources     <- sourceFiles regex
  cardProps   <- liftMaybe "Couldn't find CardProps" 
                      $ Array.head
                      $ Array.mapMaybe (\src -> interfaceByName src "CardProps") sources
  log $ showDeclarationElements (InterfaceDeclaration cardProps)
  log $ show cardProps

