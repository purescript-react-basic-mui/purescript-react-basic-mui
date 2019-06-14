module Codegen.Write.React2 where

import Prelude

import Codegen.Read (DeclarationElements(..), DeclarationSourceFile(..), EntityName(..), FunctionElementRec, InterfaceDeclarationRec, LiteralValue(..), PropertyName(..), TSType(..), TypeMember(..), TypeParameter(..), TypeReferenceRec, VariableDeclaration(..), _PropertySignature, _VariableDeclaration, _type, isOptional, liftEither, toArrayOf, typescript)
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
  , cleanFunctionName :: Regex
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
  , parentModuleName :: String
  , moduleName :: String
  , name :: String
  , path :: FilePath
  , foreignImports :: Array String
  , elements :: Array String 
  }
type JavaScript = { path :: FilePath, namespace :: String , exports :: Array String }
type PSJS = { path :: FilePath, javascript :: JavaScript, purescript :: PureScript }
type FileDescription = { fileName :: String, text :: String }

 
 
main :: Effect Unit
main = do
  regex                 <- liftEither $ Regex.regex ".*@material-ui.*" RegexFlags.noFlags
  javascriptImport      <- liftEither $ Regex.regex ".*(@material-ui.*)\\/(.*\\.d\\.ts)" RegexFlags.noFlags
  codegenPath           <- liftEither $ Regex.regex ".*@material-ui.*\\/core\\/(.*)\\.d\\.ts" RegexFlags.noFlags
  firstChar             <- liftEither $ Regex.regex "^(.)" RegexFlags.noFlags
  cleanFunctionName     <- liftEither $ Regex.regex "^[_a-z]+\\w*$" RegexFlags.noFlags
  let regexps           =  { javascriptImport, codegenPath, firstChar, cleanFunctionName }

  let typeReplacements  = 
        [ Exact "React.ComponentType" "JSX"
        --, Exists "React.Ref" "Ref"
        , Exists "CSSProperties" "CSS"
        , Exists "EventHandler" "EventHandler"
        , Exists "ReactNode" "JSX"
        , Exists "React.ElementType" "JSX"
        , Exists "React.ReactNode" "JSX"
        --, Exists "RefType" "Ref"
        , Exists "ClassNameMap" "Foreign"
        , Exists "ClassKey" "Foreign"
        , Exists "Partial" "Foreign"
        , Exists "HTML" "Foreign"
        , Exists "React.DOMAttributes" "Foreign"
        ]

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
  -- let _ = spy "state" tuple
  pure unit

write :: ReactWriter Unit
write = do
  { sources } <- Reader.ask
  traverse_ writeSource sources

writeSource :: DeclarationSourceFile -> ReactWriter Unit
writeSource source @ (DeclarationSourceFile { fileName }) = do
  Run.liftEffect $ log $ "Writing " <> fileName
  importsForSource source
  javascriptExports source
  setPathAndModuleName source
  setDeclarationElements source
  writeJavaScriptFile source
  writePureScriptFile source

importsForSource :: DeclarationSourceFile -> ReactWriter Unit
importsForSource source = do
  javascriptImports source 
  purescriptImports source
  pure unit

writePureScriptFile :: DeclarationSourceFile -> ReactWriter Unit
writePureScriptFile (DeclarationSourceFile { fileName }) = do
  psjs <- getPSJS fileName
  let modulePart = fold [ "module ", psjs.purescript.moduleName, " where " ]
  imports <- showPureScriptImports psjs
  let elementsPart = Array.intercalate "\n\n" psjs.purescript.elements
  let body = Array.intercalate "\n\n" 
        [ modulePart
        , basicImports
        , elementsPart
        ]
  Run.liftEffect $ FS.writeTextFile UTF8 psjs.purescript.path body 

basicImports :: String
basicImports = """import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)
"""

writeJavaScriptFile :: DeclarationSourceFile -> ReactWriter Unit
writeJavaScriptFile (DeclarationSourceFile { fileName }) | not $ isMUIFile fileName = pure unit 
writeJavaScriptFile (DeclarationSourceFile { fileName }) = do
  psjs <- getPSJS fileName
  let body = showJavaScriptExports psjs.javascript.namespace psjs.javascript.exports
  Run.liftEffect $ FS.writeTextFile UTF8 psjs.javascript.path body 

showJavaScriptExports :: String -> Array String -> String
showJavaScriptExports namespace exports = Array.intercalate "\n" $
  exports <#> \name -> fold [ "exports._" ,name ," = require('" ,namespace ,"')." ,name ]

showPureScriptImports :: PSJS -> ReactWriter String 
showPureScriptImports psjs = do
  let tokens = String.split (String.Pattern "\".") 
  strs <- traverse handleImport psjs.purescript.imports
  let imports = Array.intercalate "\n" $ Array.filter (\i -> i /= "") strs
  pure imports
  where
    handleImport str = do
      let tokens = String.split (String.Pattern "\".") str
      let moduleBase = tokens Array.!! 0 
      let name = tokens Array.!! 1 
      handleName moduleBase name      

    handleName :: Maybe String -> Maybe String -> ReactWriter String
    handleName (Just moduleBase) (Just name) | (moduleBase <> name) == (psjs.purescript.moduleName <> psjs.purescript.name)  = pure ""
    handleName (Just moduleBase) (Just name)  = do
      { moduleName, parentModuleName } <- getNames moduleBase
      pure $ fold [ "import ", moduleName, name, " (", name, ")" ]
    handleName _ _ = pure ""
      


setDeclarationElements :: DeclarationSourceFile -> ReactWriter Unit
setDeclarationElements (DeclarationSourceFile {fileName, elements}) = do
  strs <- traverse showDeclarationElements elements
  let filtered = strs # Array.filter \str -> str /= ""
  psjs <- getPSJS fileName
  setPSJS psjs { purescript = psjs.purescript { elements = filtered } }

showDeclarationElements :: DeclarationElements -> ReactWriter String
showDeclarationElements (FunctionElement rec) = showFunctionElement rec
showDeclarationElements (VariableStatement declarations) = showVariableDeclarations declarations
showDeclarationElements (InterfaceDeclaration rec) = showInterfaceDeclaration rec
showDeclarationElements elem = pure "" 

showInterfaceDeclaration :: InterfaceDeclarationRec -> ReactWriter String
showInterfaceDeclaration { name, typeParameters, typeMembers } = do
  let partition = Array.partition isOptional typeMembers 
  let optionalMembers = partition.yes
  let requiredMembers = partition.no
  Array.intercalate "\n\n" <$> sequence [ allType typeMembers, requiredType requiredMembers, optionalType optionalMembers ] 
  where
    typeParametersStr | Array.length typeParameters > 0 = " " <> (Array.intercalate " " $ map showTypeParameter typeParameters)
    typeParametersStr = ""
    requiredType members = (showMembers members) <#> \body ->  ("type " <> name <> "_required" <> typeParametersStr <> " =\n  ( " <> body <> "\n  )")
    optionalType members = (showMembers members) <#> \body ->  ("type " <> name <> "_optional" <> typeParametersStr <> " =\n  ( " <> body <> "\n  )")
    allType members = (showMembers members) <#> \body ->  ("type " <> name <> " " <> typeParametersStr <> " =\n  { " <> body <> "\n  }")

    showMembers :: Array TypeMember -> ReactWriter String
    showMembers members = do 
      strs <- traverse showTypeMember members
      pure $ Array.intercalate "\n  , " $ strs 

showTypeParameter :: TypeParameter -> String
showTypeParameter (TypeParameter param) = String.toLower param

showTypeMember :: TypeMember -> ReactWriter String
showTypeMember (PropertySignature (signature @ { name : Just name })) = do
  { regexps } <- Reader.ask
  fieldType <- showTSType signature.type
  pure $ ((clean regexps.cleanFunctionName $ showPropertyName name) <> " :: " <> fieldType)
  where
    clean :: Regex -> String -> String
    clean regex name = case (Regex.match regex name) of 
      Nothing -> "\"" <> name <> "\""
      Just _ -> name

showTypeMember (PropertySignature signature) = showTSType signature.type
showTypeMember typeMember = pure $ show typeMember

showPropertyName :: PropertyName -> String
showPropertyName (IdentifierName name) = name
showPropertyName (StringLiteral name) = name
showPropertyName (NumericLiteral name) = show (show name)

showVariableDeclarations :: Array VariableDeclaration -> ReactWriter String
showVariableDeclarations = (map $ Array.intercalate "\n\n") <<< traverse showVariableDeclaration 

showVariableDeclaration :: VariableDeclaration -> ReactWriter String
showVariableDeclaration (VariableDeclaration rec) = do
  lower <- lowerCaseFirst rec.name
  let unfiltered = showUnfilteredTSType rec.type
  if isReactComponent
    then showReactComponent
    else showRegular 

  where 
    isReactComponent = showUnfilteredTSType rec.type == "React.ComponentType"

    showRegular = do 
      lower <- lowerCaseFirst rec.name
      _type <- showTSType rec.type
      let body = fold [ lower, " = _", rec.name ]
      let foreignData = fold [ "foreign import _", rec.name, " :: ", _type ]
      pure $ fold [ lower , " :: " , _type, "\n", body, "\n", foreignData ]   

    showReactComponent = do
      lower <- lowerCaseFirst rec.name
      _type <- showTSType rec.type
      let signature = fold 
            [ lower
            , "\n  :: ∀ attrs attrs_\n   . Union attrs attrs_ ("
            , rec.name
            , "Props_optional)\n  => Record ("
            , rec.name
            , "Props_required attrs)\n  -> JSX"
            ]
      let body = fold [ lower, " = element _", rec.name ]
      let foreignData = fold [ "foreign import _", rec.name, " :: forall a. Component a " ]
      pure $ fold [ signature, "\n", body, "\n", foreignData ]



showFunctionElement :: FunctionElementRec -> ReactWriter String
showFunctionElement { name : Nothing } = pure ""
showFunctionElement { name : Just name, typeParameters, parameters, returnType } = do
  _type <- showTSType returnType
  pure $ fold 
    [ name
    , " :: Foreign -> "
    , _type
    , "\n"
    , name
    , " = _"
    , name
    , "\n"
    , "foreign import _"
    , name
    , " :: Foreign -> "
    , _type
    ]


replaceType :: String -> ReactWriter String
replaceType _type = do
  { typeReplacements } <- Reader.ask
  pure $ fromMaybe _type $ Array.foldl replace Nothing typeReplacements
  where
    replace :: Maybe String -> TypeReplacement -> Maybe String 
    replace (Just value) _ = Just value
    replace _ (Exists comp value) | isJust $ String.indexOf (String.Pattern comp) _type = Just value
    replace _ (RegexReplace regex value) | isJust $ Regex.match regex _type = Just value
    replace _ (Exact comp value) | _type == comp = Just value
    replace _ _ = Nothing

showTSType ::  TSType -> ReactWriter String
showTSType tsType = do
  { typeReplacements } <- Reader.ask
  replaceType =<< tmpTypeReplacement tsType

tmpTypeReplacement :: TSType -> ReactWriter String
tmpTypeReplacement (_type @ (TypeReference { name })) = do 
  { typeReplacements }  <- Reader.ask
  let entityName        = showEntityName name
  replacement           <- replaceType entityName 
  if replacement /= entityName
    then pure replacement
    else pure "Foreign"
tmpTypeReplacement _type = pure $ showUnfilteredTSType _type

  

showUnfilteredTSType :: TSType -> String
showUnfilteredTSType BooleanType = "Boolean"
showUnfilteredTSType StringType = "String"
showUnfilteredTSType NumberType = "Number"
showUnfilteredTSType BigIntType = "Number"
showUnfilteredTSType ObjectType = "(Object Foreign)"
showUnfilteredTSType VoidType = "Unit"
showUnfilteredTSType AnyType = "Foreign"
showUnfilteredTSType SymbolType = "Foreign"
showUnfilteredTSType UnknownType = "Foreign"
showUnfilteredTSType NeverType = "Foreign"
--    showUnfilteredTSType (ParenthesizedType t) = "(" <> (showTSType replacements t) <> ")"
--    showUnfilteredTSType (TypeReference { name, typeArguments }) | Array.length typeArguments > 0 = showEntityName name <> " " <> (Array.intercalate " " $ map (showTSType replacements) typeArguments)
--    showUnfilteredTSType (TypeReference { name, typeArguments }) = showEntityName name
--    showUnfilteredTSType (FunctionType { typeParameters, parameters, returnType }) | Array.length typeParameters == 0 = (Array.intercalate " -> " $ map (showTypeMemberAsFunctionType replacements) parameters) <> " -> " <> (showTSType  replacements returnType)
--    showUnfilteredTSType (FunctionType { typeParameters, parameters, returnType }) = "forall " <> (Array.intercalate " " $ map showTypeParameter typeParameters) <> ". " <> (Array.intercalate " -> " $ map (showTypeMemberAsFunctionType replacements ) parameters) <> " -> " <> (showTSType replacements returnType)
--    showUnfilteredTSType (ArrayType t) = "(Array " <> (showTSType replacements t) <> ")"
--    showUnfilteredTSType (UnionType types) = handleUnionTypes types
showUnfilteredTSType (LiteralType (LiteralStringValue value)) = "String"
showUnfilteredTSType (LiteralType (LiteralNumericValue value)) = "Number"
showUnfilteredTSType (LiteralType (LiteralBigIntValue value)) = "Number"
showUnfilteredTSType (LiteralType (LiteralBooleanValue value)) = "Boolean"
-- showUnfilteredTSType (TypeLiteral members) = "{ " <> (Array.intercalate ", " $ map showTypeMember members) <> " }"
showUnfilteredTSType t = "Foreign"

getNames :: String -> ReactWriter { name :: String, moduleName :: String, parentModuleName :: String }
getNames fileName = do
  { regexps, outputRoot } <- Reader.ask
  let match       =  Regex.match regexps.codegenPath fileName 
  let splitter    =  String.split (String.Pattern "/")
  tokens          <- Array.nubEq <$> (traverse upperCaseFirst $ maybe [] splitter $ preview (_Just <<< ix 1 <<< _Just) match)
  let pathPrefix  =  Array.intercalate "/" tokens
  let parentModuleName = "React.Basic.MUI." <> (maybe "" (Array.intercalate "." ) $ Array.init tokens)
  let name = fromMaybe "" $ Array.head =<< Array.tail tokens
  let moduleName  =  "React.Basic.MUI." <> (Array.intercalate "." tokens) 
  pure { name, moduleName, parentModuleName }


setPathAndModuleName :: DeclarationSourceFile -> ReactWriter Unit 
setPathAndModuleName (DeclarationSourceFile { fileName }) = do
  { regexps, outputRoot } <- Reader.ask
  let match       =  Regex.match regexps.codegenPath fileName 
  let splitter    =  String.split (String.Pattern "/")
  tokens          <- Array.nubEq <$> (traverse upperCaseFirst $ maybe [] splitter $ preview (_Just <<< ix 1 <<< _Just) match)
  mkDir outputRoot tokens
  let pathPrefix  =  Array.intercalate "/" tokens
  jsPath          <- Run.liftEffect $ Path.resolve [outputRoot] $ pathPrefix <> ".js"
  psPath          <- Run.liftEffect $ Path.resolve [outputRoot] $ pathPrefix <> ".purs"
  let parentModuleName = "React.Basic.MUI." <> (maybe "" (Array.intercalate "." ) $ Array.init tokens)
  let name = fromMaybe "" $ Array.head =<< Array.tail tokens
  let moduleName  =  "React.Basic.MUI." <> (Array.intercalate "." tokens) 
  psjs          <- getPSJS fileName 
  setPSJS (psjs { javascript = psjs.javascript { path = jsPath }, purescript = psjs.purescript { path = psPath, name = name, moduleName = moduleName, parentModuleName = parentModuleName } })
    where
      mkDir :: FilePath -> Array String -> ReactWriter Unit
      mkDir _ tokens | Array.length tokens <= 1 = pure unit
      mkDir outputRoot tokens = do
        path    <- Run.liftEffect $ Path.resolve [outputRoot] $ maybe outputRoot (Array.intercalate "/") $ Array.init tokens
        exists  <- Run.liftEffect $ FS.exists path 
        if not exists then Run.liftEffect $ FS.mkdir path else pure unit
 

javascriptImports :: DeclarationSourceFile -> ReactWriter Unit
javascriptImports (DeclarationSourceFile { fileName, elements }) | not $ isMUIFile fileName = pure unit 
javascriptImports (DeclarationSourceFile { fileName, elements }) = do
  { regexps }   <- Reader.ask
  let matches   = Regex.match regexps.javascriptImport fileName 
  let namespace = fromMaybe "" $ preview (_Just <<< ix 1 <<< _Just) matches
  psjs          <- getPSJS fileName
  setPSJS psjs { javascript { namespace = namespace } }
      

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
  let types           = extractTypeNames typeReferences
  replaced            <- Array.nubEq <$> traverse replaceType types
  imports             <- Array.filterA (canImport extractNames) replaced 
  foreignImports      <- Array.filterA ((map not) <<< (canImport extractNames)) replaced
  psjs <- getPSJS fileName
  setPSJS psjs { purescript { imports = imports, foreignImports = foreignImports } }
  where
    extractNames :: Array String
    extractNames = join $ map getName elements 
      where
        getName :: DeclarationElements -> Array String
        getName (InterfaceDeclaration rec) = [ rec.name, fromMaybe "" rec.fullyQualifiedName ]
        getName (FunctionElement rec) = [ fromMaybe "" rec.name, fromMaybe "" rec.fullyQualifiedName ]
        getName (VariableStatement declarations) = join $ declarations <#> (\(VariableDeclaration { name, fullyQualifiedName }) -> [ name, fromMaybe "" fullyQualifiedName ])
        getName _ = []

       

    extractTypeNames :: Array TypeReferenceRec -> Array String
    extractTypeNames = map extractTypeName 

    extractTypeName :: TypeReferenceRec -> String 
    extractTypeName { fullyQualifiedName: (Just fullyQualifiedName) } | (fullyQualifiedName /= "__type" && fullyQualifiedName /= "__type.bivarianceHack") = fullyQualifiedName
    extractTypeName { name } = showEntityName name

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

    canImport :: Array String -> String -> ReactWriter Boolean
    canImport names name = do
      { types } <- Reader.ask
      let isAvailable = isJust $ Object.lookup name types 
      let isDefinedInThisFile = Array.elem name names
      pure $ isAvailable && (not isDefinedInThisFile)

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


 