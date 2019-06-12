module Codegen.Main where

import Prelude

import Codegen.Read (DeclarationSourceFile(..), liftEither, liftMaybe, typescript)
import Codegen.Write.React (showDeclarationSourceFile)
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
  regex       <- liftEither $ Regex.regex ".*Avatar.*" RegexFlags.noFlags
  let path    =  "./node_modules/@material-ui/core/index.d.ts"
  { sources } <- typescript path regex 
  badge       <- liftMaybe "Couldn't find file" 
                  $ Array.find (\(DeclarationSourceFile { fileName }) -> isJust $ String.indexOf (String.Pattern "/Avatar.d.ts") fileName) sources
  str         <- showDeclarationSourceFile badge
  log str

getVariant :: Variant (foo :: Unit, bar :: Unit)
getVariant = inj (SProxy :: SProxy "bar") unit 