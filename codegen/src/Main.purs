module Codegen.Main where

import Prelude

import Codegen.Read (DeclarationSourceFile(..), interfaceByName, liftEither, liftMaybe, sourceFiles)
import Data.Array as Array
import Data.Maybe (isJust)
import Data.String.Regex as String
import Data.String.Regex.Flags as RegexFlags
import Effect (Effect)
import Effect.Console (log)


main :: Effect Unit
main = do
  sources       <- sourceFiles
  avatarRegex   <- liftEither $ String.regex "\\/Avatar.d.ts$" RegexFlags.noFlags
  cardRegex     <- liftEither $ String.regex "\\/Card.d.ts$" RegexFlags.noFlags
  avatar        <- liftMaybe "Couldn't find Avatar" $ Array.head $ Array.filter (\(DeclarationSourceFile { fileName }) -> isJust $ String.match avatarRegex fileName) sources
  card          <- liftMaybe "Couldn't find Card" $ Array.head $ Array.filter (\(DeclarationSourceFile { fileName }) -> isJust $ String.match cardRegex fileName) sources
  cardProps     <- liftMaybe "Couldn't find CardProps" $ interfaceByName card "CardProps"
  log $ show cardProps

