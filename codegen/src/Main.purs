module Codegen.Main where

import Prelude

import Codegen.Read (DeclarationSourceFile(..), interfaceByName, liftEither, liftMaybe, sourceFiles)
import Codegen.Write.React (showPropsInterface)
import Data.Array as Array
import Data.Maybe (isJust)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Variant (SProxy(..), Variant, inj)
import Effect (Effect)
import Effect.Console (log)


main :: Effect Unit
main = do
  regex       <- liftEither $ Regex.regex ".*Badge.*" RegexFlags.noFlags
  --regex       <- liftEither $ Regex.regex ".*react/index.*" RegexFlags.noFlags
  sources     <- sourceFiles regex
  badgeProps  <- (liftMaybe "Couldn't find BadgeProps" 
                  $ Array.head
                  $ Array.mapMaybe (\src -> interfaceByName src "BadgeProps") sources)
  badge       <- liftMaybe "Couldn't find Badge.d.ts" 
                  $ Array.find (\(DeclarationSourceFile { fileName }) -> isJust $ String.indexOf (String.Pattern "/Badge.d.ts") fileName) sources
  log $ showPropsInterface badgeProps
  log $ show badge

getVariant :: Variant (foo :: Unit, bar :: Unit)
getVariant = inj (SProxy :: SProxy "bar") unit 