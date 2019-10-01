module Codegen where

import Prelude

import Codegen.AST.Printers (printModule)
import Codegen.Model (Component, ModulePath(..))
import Codegen.Model (ModulePath) as Model
import Codegen.TS.MUI (componentAST) as TS.MUI
import Control.Monad.Except (runExceptT)
import Data.Either (Either(..))
import Data.String (joinWith)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (exists, mkdir, writeTextFile) as FS
import Node.Path (FilePath)

data Codegen = Codegen File String

data File
  = File String
  | Directory String File

pureScriptFile :: Model.ModulePath -> File
pureScriptFile (Path str next) = Directory str $ pureScriptFile next
pureScriptFile (Name name) = File $ name <> ".purs"

javaScriptFile :: Model.ModulePath -> File
javaScriptFile (Path str next) = Directory str $ javaScriptFile next
javaScriptFile (Name name) = File $ name <> ".js"

genPureScript :: Component -> Effect Unit -- Array Codegen
genPureScript component = do
  result ← runExceptT $ TS.MUI.componentAST $ component
  case result of
    Right m → log $ printModule m
    Left err → log $ joinWith ", " err

-- genForeign :: Component -> String
-- genForeign { name } = "foreign import _" <> name <> " :: ∀ a. ReactComponent a"

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
        else FS.mkdir path

codegen :: Component -> Effect Unit
codegen component =
  (genPureScript component) -- <> (genJavaScript component)

