module Codegen.Main where

import Prelude

import Codegen.Read (DeclarationElements(..), DeclarationSourceFile(..), interfaceByName, liftEither, liftMaybe, showDeclarationElements, sourceFiles)
import Data.Array as Array
import Data.Maybe (isJust)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Effect (Effect)
import Effect.Console (log)


main :: Effect Unit
main = do
  regex       <- liftEither $ Regex.regex ".*material-ui.*" RegexFlags.noFlags
  sources     <- sourceFiles regex
  cardProps   <- (liftMaybe "Couldn't find CardProps" 
                  $ Array.head
                  $ Array.mapMaybe (\src -> interfaceByName src "CardProps") sources)
  avatar      <- liftMaybe "Couldn't find Avatar.d.ts" 
                  $ Array.find (\(DeclarationSourceFile { fileName }) -> isJust $ String.indexOf (String.Pattern "/Avatar.d.ts") fileName) sources
  log $ showDeclarationElements (InterfaceDeclaration cardProps)
  log $ show avatar

