module Codegen.Write.React2 where

import Prelude

import Codegen.Read (DeclarationElements(..), DeclarationSourceFile(..), EntityName(..), TSType(..), TypeReferenceRec, VariableDeclaration(..), _PropertySignature, _VariableDeclaration, _type, liftEither, toArrayOf, typescript)
import Data.Array (fold)
import Data.Array as Array
import Data.Lens (_Just, preview, traversed)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing, maybe)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (sequence, traverse, traverse_)
import Debug.Trace (spy)
import Effect (Effect)
import Effect.Console (log)
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import Run (Run, EFFECT)
import Run as Run
import Run.Reader (READER)
import Run.Reader as Reader
import Run.State (STATE)
import Run.State as State

type Context = 
  { typeReplacements :: Array TypeReplacement
  , sources :: Array DeclarationSourceFile 
  , types :: Object Foreign
  , regexps :: Regexps
  , outputRoot :: String
  }
type State = {
  files :: Object PSJS
}

type Regexps = 
  { javascriptImport :: Regex
  , codegenPath :: Regex
  , firstChar :: Regex
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
  { imports :: Array String
  , moduleName :: String
  , path :: FilePath
  , foreignImports :: Array String
  , elements :: Array TSType
  }
type JavaScript = { path :: FilePath, imports :: Array String , exports :: Array String }
type PSJS = { path :: FilePath, javascript :: JavaScript, purescript :: PureScript }
type FileDescription = { fileName :: String, text :: String }

 
 
main :: Effect Unit
main = do
  regex                 <- liftEither $ Regex.regex ".*Card.*" RegexFlags.noFlags
  javascriptImport      <- liftEither $ Regex.regex ".*(@material-ui.*)\\/(.*\\.d\\.ts)" RegexFlags.noFlags
  codegenPath           <- liftEither $ Regex.regex ".*@material-ui.*\\/core\\/(.*)\\.d\\.ts" RegexFlags.noFlags
  firstChar             <- liftEither $ Regex.regex "^(.)" RegexFlags.noFlags
  let regexps           =  { javascriptImport, codegenPath, firstChar }
  let path              =  "./node_modules/@material-ui/core/index.d.ts"
  cwd                   <- Process.cwd
  outputRoot            <- Path.resolve [cwd] "../src/"
  { sources, types }    <- typescript path regex
  let filteredSources   =  Array.filter (\(DeclarationSourceFile { fileName }) -> isNothing $ String.indexOf (String.Pattern "index.d.ts") fileName) sources
  let context           =  { sources : filteredSources, types, typeReplacements, regexps, outputRoot }
  let state             =  mempty
  tuple                 <- Run.runBaseEffect
                            $ State.runState state
                            $ Reader.runReader context write
  let _ = spy "state" tuple
  pure unit

write :: ReactWriter Unit
write = do
  { sources } <- Reader.ask
  traverse_ writeSource sources


writeSource :: DeclarationSourceFile -> ReactWriter Unit
writeSource source = do
  importsForSource source
  javascriptExports source
  setPathAndModuleName source
  writeJavaScriptFile source


importsForSource :: DeclarationSourceFile -> ReactWriter Unit
importsForSource source = do
  javascriptImports source 
  purescriptImports source
  pure unit

writeJavaScriptFile :: DeclarationSourceFile -> ReactWriter Unit
writeJavaScriptFile (DeclarationSourceFile { fileName }) = do
  psjs <- getPSJS fileName
  let body = Array.intercalate "\n" psjs.javascript.imports
  Run.liftEffect $ FS.writeTextFile UTF8 psjs.javascript.path body 

setPathAndModuleName :: DeclarationSourceFile -> ReactWriter Unit 
setPathAndModuleName (DeclarationSourceFile { fileName }) = do
  { regexps, outputRoot } <- Reader.ask
  let match       =  Regex.match regexps.codegenPath fileName 
  let splitter    =  String.split (String.Pattern "/")
  tokens          <- Array.nubEq <$> (traverse upperCaseFirst $ maybe [] splitter $ preview (_Just <<< ix 1 <<< _Just) match)
  let pathPrefix  =  Array.intercalate "/" tokens
  jsPath          <- Run.liftEffect $ Path.resolve [outputRoot] $ pathPrefix <> ".js"
  psPath          <- Run.liftEffect $ Path.resolve [outputRoot] $ pathPrefix <> ".purs"
  let moduleName  =  "React.Basic.MUI." <> (Array.intercalate "." tokens) 
  psjs          <- getPSJS fileName 
  setPSJS (psjs { javascript = psjs.javascript { path = jsPath }, purescript = psjs.purescript { path = psPath, moduleName = moduleName } })
  Run.liftEffect $ log (Array.intercalate "\n" [jsPath, psPath, moduleName])
 

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

javascriptExports :: DeclarationSourceFile -> ReactWriter Unit
javascriptExports (DeclarationSourceFile { fileName, elements }) = do
  let exports = Array.nub $ join $ map extract elements 
  psjs <- getPSJS fileName
  setPSJS psjs { javascript { exports = exports } }
  pure unit
  where
    extract :: DeclarationElements -> Array String
    extract (ExportAssignment (Just name)) = [ name ]
    extract (FunctionElement rec) = maybe [] pure rec.name
    extract (VariableStatement declarations) = map (\(VariableDeclaration rec) -> rec.name) declarations
    extract _ = []
 

purescriptImports :: DeclarationSourceFile -> ReactWriter Unit
purescriptImports (DeclarationSourceFile { fileName, elements }) = do
  let typeReferences  = join $ map handleElement elements
  let types           = Array.nubEq $ extractNames typeReferences
  imports             <- Array.filterA canImport types  
  foreignImports      <- Array.filterA ((map not) <<< canImport) types  
  psjs <- getPSJS fileName
  setPSJS psjs { purescript { imports = imports, foreignImports = foreignImports } }
  where
    extractNames :: Array TypeReferenceRec -> Array String
    extractNames = map extractName 

    extractName :: TypeReferenceRec -> String 
    extractName { fullyQualifiedName: (Just fullyQualifiedName) } | (fullyQualifiedName /= "__type" && fullyQualifiedName /= "__type.bivarianceHack") = fullyQualifiedName
    extractName { name } = showEntityName name

    handleElement :: DeclarationElements -> Array TypeReferenceRec
    handleElement (InterfaceDeclaration rec) =
      join $ map extractTypes $ toArrayOf (traversed <<< _PropertySignature <<< _type) rec.typeMembers
    handleElement (FunctionElement rec) = 
      join $ map extractTypes $ toArrayOf (traversed <<< _PropertySignature <<< _type) rec.parameters
    handleElement (VariableStatement declarations) = do
      join $ map extractTypes $ toArrayOf (traversed <<< _VariableDeclaration <<< _type) declarations
    handleElement _ = []

    extractTypes :: TSType -> Array TypeReferenceRec
    extractTypes (ArrayType t) = extractTypes t
    extractTypes (IntersectionType t) = join $ map extractTypes t
    extractTypes (UnionType ts) = join $ map extractTypes ts
    extractTypes (TupleType ts) = join $ map extractTypes ts
    extractTypes (ConditionalType { checkType, extendsType, trueType, falseType }) = join [extractTypes checkType, extractTypes extendsType ]
    extractTypes (IndexAccessType { indexType, objectType }) = join [ extractTypes indexType, extractTypes objectType ]
    extractTypes (MappedType rec ) = extractTypes rec.type
    extractTypes (ParenthesizedType t) = extractTypes t
    extractTypes (ConstructorType { parameters, returnType }) = do
      let extractedTypes = toArrayOf (traversed <<< _PropertySignature <<< _type) parameters 
      join [ extractTypes returnType, (join $ map extractTypes extractedTypes) ]
    extractTypes (FunctionType { parameters, returnType }) = do
      let extractedTypes = toArrayOf (traversed <<< _PropertySignature <<< _type) parameters 
      join [ extractTypes returnType, (join $ map extractTypes extractedTypes) ]
    extractTypes (TypeLiteral members) = do
      join $ map extractTypes $ toArrayOf (traversed <<< _PropertySignature <<< _type) members
    extractTypes (TypeReference t) = Array.cons t $ join $ map extractTypes t.typeArguments
    extractTypes _ = [] 

    canImport :: String -> ReactWriter Boolean
    canImport name = do
      { types } <- Reader.ask
      pure $ isJust $ Object.lookup name types 

lowerCaseFirst :: String -> ReactWriter String
lowerCaseFirst str = do
  { regexps } <- Reader.ask
  let replaced = Regex.replace' regexps.firstChar (\s _ -> String.toLower s) str
  pure replaced

upperCaseFirst :: String -> ReactWriter String
upperCaseFirst str = do
  { regexps } <- Reader.ask
  let replaced = Regex.replace' regexps.firstChar (\s _ -> String.toUpper s) str
  pure replaced
    

showEntityName :: EntityName -> String
showEntityName (Identifier name) = name
showEntityName (QualifiedName { left, right }) = (showEntityName left) <> "." <> right

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
 