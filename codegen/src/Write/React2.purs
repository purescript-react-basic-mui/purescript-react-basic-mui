module Codegen.Write.React2 where

import Prelude

import Codegen.Read (DeclarationElements(..), DeclarationSourceFile(..), EntityName(..), TSType(..), TypeReferenceRec, VariableDeclaration(..), _PropertySignature, _VariableDeclaration, _type, liftEither, toArrayOf, typescript)
import Data.Array as Array
import Data.Lens (_Just, preview, traversed)
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), isJust, isNothing, maybe)
import Data.String as String
import Data.String.Regex (Regex)
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.Traversable (traverse_)
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
  , types :: Object Foreign
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
  { imports :: Array String
  , foreignImports :: Array String
  , elements :: Array TSType
  }
type JavaScript = { imports :: Array String , exports :: Array String }
type PSJS = { path :: FilePath, javascript :: JavaScript, purescript :: PureScript }
type FileDescription = { fileName :: String, text :: String }

writeJavascript :: ReactWriter Unit
writeJavascript = do
  { files } <- State.get
  traverse_ writeJS files
  where
    writeJS :: PSJS -> ReactWriter Unit 
    writeJS { path, javascript } = do
      Run.liftEffect $ traverse_ (\i -> log (path <> "\n" <> i)) javascript.imports
      pure unit
 
main :: Effect Unit
main = do
  regex                 <- liftEither $ Regex.regex ".*Card.*" RegexFlags.noFlags
  javascriptImport      <- liftEither $ Regex.regex ".*(@material-ui.*)\\/(.*\\.d\\.ts)" RegexFlags.noFlags
  let regexps           =  { javascriptImport }
  let path              =  "./node_modules/@material-ui/core/index.d.ts"
  { sources, types }    <- typescript path regex
  let filteredSources = Array.filter (\(DeclarationSourceFile { fileName }) -> isNothing $ String.indexOf (String.Pattern "index.d.ts") fileName) sources
  let context = { sources : filteredSources, types, typeReplacements, regexps }
  let state = mempty
  tuple                 <- Run.runBaseEffect
                            $ State.runState state
                            $ Reader.runReader context write
  pure unit

write :: ReactWriter Unit
write = do
  { sources } <- Reader.ask
  traverse_ importsForSource sources
  writeJavascript

importsForSource :: DeclarationSourceFile -> ReactWriter Unit
importsForSource source = do
  javascriptImports source 
  purescriptImports source
  javascriptExports source
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
 