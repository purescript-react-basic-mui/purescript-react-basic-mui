module Codegen where

import Prelude

import Codegen.AST.Printers (printModule)
import Codegen.Model (Component, ModulePath(..))
import Codegen.Model (ModulePath) as Model
import Codegen.TS.MUI (componentAST) as TS.MUI
import Codegen.TS (M) as TS
import Data.String (toLower) as String
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, writeTextFile) as FS
import Node.FS.Extra (ensureDir)
import Node.Path (FilePath)

data Codegen = Codegen File String

data File
  = File String
  | Directory String File

type M a = TS.M a

pureScriptFile :: Model.ModulePath -> File
pureScriptFile (Path str next) = Directory str $ pureScriptFile next
pureScriptFile (Name name) = File $ name <> ".purs"

javaScriptFile :: Model.ModulePath -> File
javaScriptFile (Path str next) = Directory str $ javaScriptFile next
javaScriptFile (Name name) = File $ name <> ".js"

genPureScript :: Component -> M String
genPureScript component =
  TS.MUI.componentAST component <#> printModule

genJavaScript :: Component -> String
genJavaScript { name, modulePath } =
  "exports._" <> name <> " = require(\"@material-ui/" <> (jsPath modulePath) <> "\").default;"
  where
    jsPath (Path str next) | str /= "MUI" = (String.toLower str) <> "/" <> (jsPath next)
    jsPath (Path str next) = (jsPath next)
    jsPath (Name n) = n

write :: FilePath -> Codegen -> Aff Unit
write basePath (Codegen file code) = go basePath file
  where
    go path (Directory name next) = go (path <> "/" <> name) next
    go path (File name) = do
      mkDir path
      let filePath = path <> "/" <> name
      liftEffect $ log ("Writing: " <> filePath)
      FS.writeTextFile UTF8 filePath code
    mkDir path = do
      exists <- FS.exists path
      if exists
        then pure unit
        else ensureDir path

codegen :: Component -> TS.M { ps ∷ String, js ∷ String }
codegen component =
  { ps: _, js: genJavaScript component} <$> (genPureScript component)

