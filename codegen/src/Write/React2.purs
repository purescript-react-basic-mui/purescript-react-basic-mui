module Codegen.Write.React2 where

import Prelude

import Codegen.Read (DeclarationElements, DeclarationSourceFile(..), TSType, liftEither, typescript)
import Data.Array as Array
import Data.Lens (_Just, preview)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), isJust, maybe)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as Flags
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (traverse, traverse_)
import Data.Tuple (Tuple(..))
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console (log)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Path (FilePath)
import Run (Run, EFFECT)
import Run as Run
import Run.Reader (READER)
import Run.Reader as Reader
import Run.State (STATE)
import Run.State as State

type Context = 
  { typeReplacements :: Array TypeReplacement
  , sources :: Array DeclarationSourceFile 
  , elements :: Object Foreign
  , regexps :: Regexps
  }
type State = {
  files :: Object PSJS
}

type Regexps = 
  { javascriptImport :: Regex
  }

type ReactWriter a = Run(reader :: READER Context, state :: STATE State, effect :: EFFECT) a

data PureScriptElement
  = NormalFunction DeclarationElements
  | UnionFunction DeclarationElements

data TypeReplacement
  = Exists String String
  | RegexReplace Regex String
  | Exact String String

type PureScript = 
  { imports :: Array TSType
  , foreignImports :: Array DeclarationElements
  , exports :: Array DeclarationElements
  , elements :: Array TSType
  }
type JavaScript = { imports :: Array String , exports :: Array String }
type PSJS = { path :: FilePath, javascript :: JavaScript, purescript :: PureScript }
type FileDescription = { fileName :: String, text :: String }

main :: Effect Unit
main = do
  regex                 <- liftEither $ Regex.regex ".*Avatar.*" RegexFlags.noFlags
  javascriptImport      <- liftEither $ Regex.regex ".*(@material-ui.*)\\/(.*\\.d\\.ts)" RegexFlags.noFlags
  let regexps           =  { javascriptImport }
  let path              =  "./node_modules/@material-ui/core/index.d.ts"
  { sources, elements } <- typescript path regex
  let context = { sources, elements, typeReplacements, regexps }
  let state = mempty
  tuple                 <- Run.runBaseEffect
                            $ State.runState state
                            $ Reader.runReader context write
  let _ = spy "tuple" tuple
  pure unit

write :: ReactWriter Unit
write = imports


imports :: ReactWriter Unit
imports = do
  { sources } <- Reader.ask
  traverse_ importsForSource sources

importsForSource :: DeclarationSourceFile -> ReactWriter Unit
importsForSource source = do
  javascriptImports source 
  purescriptImports source
  pure unit

javascriptImports :: DeclarationSourceFile -> ReactWriter Unit
javascriptImports (DeclarationSourceFile { fileName, elements }) | not $ isMUIFile fileName = pure unit 
javascriptImports (DeclarationSourceFile { fileName, elements }) = do
  { regexps } <- Reader.ask
  let matches = Regex.match regexps.javascriptImport fileName 
  let namespace = preview (_Just <<< ix 1 <<< _Just) matches
  maybe (pure unit) addNamespace namespace
  where
    addNamespace namespace = getPSJS fileName >>= \psjs -> 
      setPSJS psjs { javascript { imports = Array.cons namespace psjs.javascript.imports } }

purescriptImports :: DeclarationSourceFile -> ReactWriter Unit
purescriptImports (DeclarationSourceFile { fileName, elements }) = do
  Run.liftEffect $ log $ show elements
  pure unit


isMUIFile :: FilePath -> Boolean
isMUIFile path = isJust $ String.indexOf (String.Pattern "@material-ui") path

getPSJS :: FilePath -> ReactWriter PSJS
getPSJS path = do
  state <- State.get
  pure $ maybe ({ path, javascript: mempty, purescript: mempty }) identity (Object.lookup path state.files)

setPSJS :: PSJS -> ReactWriter Unit
setPSJS psjs = do 
  state <- State.get
  let updatedFiles = Object.insert psjs.path psjs state.files 
  State.put state { files = updatedFiles }

typeReplacements :: Array TypeReplacement
typeReplacements = []
 