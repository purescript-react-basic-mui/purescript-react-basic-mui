module Test.Main where

import Prelude

import Codegen.Main (main) as Main
import Codegen.Read (liftEither, typescript)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Effect (Effect)
import Effect.Console (log)

test :: Effect Unit
test = do
  let fileName = "test.d.ts"
  regex       <- liftEither $ Regex.regex ".*test.*" RegexFlags.noFlags
  { sources } <- typescript fileName regex 
  log $ show sources 
 
main :: Effect Unit
main = Main.main 
-- main = test
  