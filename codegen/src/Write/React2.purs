module Codegen.Write.React2 where

import Prelude

import Codegen.Read (DeclarationElements(..), DeclarationSourceFile(..), EntityName(..), FunctionElementRec, InterfaceDeclarationRec, LiteralValue(..), PropertyName(..), TSType(..), TypeAliasDeclarationRec, TypeMember(..), TypeParameter(..), TypeReferenceRec, VariableDeclaration(..), _PropertySignature, _VariableDeclaration, _isOptional, _type, interfaces, isOptional, liftEither, toArrayOf, typescript)
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
import Data.Tuple (Tuple(..), snd)
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
, hasRequiredFields :: Object Boolean
, reverseImports :: Object String
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
  { imports :: Object (Array String)
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
        [ Exact "T" "Foreign"
        , Exact "P" "Foreign"
        , Exact "R" "Foreign"
        , Exact "D" "Foreign"
        , Exact "M" "Foreign"
        , Exact "Key" "Foreign"
        , Exact "Props" "Foreign"
        , Exact "Exclude" "Foreign"
        , Exact "Styles" "Foreign"
        , Exists "React.ComponentType" "JSX"
        , Exists "Hook" "Foreign"
        , Exists "CSSProperties" "CSS"
        , Exists "Handler" "EventHandler"
        , Exists "ReactNode" "JSX"
        , Exists "React.ElementType" "JSX"
        , Exists "React.ReactNode" "JSX"
        , Exists "RefType" "Foreign"
        , Exists "React.Ref" "Foreign"
        , Exists "AcceptsRef" "Foreign"
        , Exists "ClassNameMap" "Foreign"
        , Exists "ClassKey" "Foreign"
        , Exists "Partial" "Foreign"
        , Exists "HTML" "Foreign"
        , Exists "React.DOMAttributes" "Foreign"
        , Exists "React" "Foreign"
        , Exists "PropTypes" "Foreign"
        , Exists "Component" "ReactComponent"
        , Exists "LegacyRef" "Foreign"
        , Exists "Color" "Foreign"
        ]

  let path              =  "./node_modules/@material-ui/core/index.d.ts"
  cwd                   <- Process.cwd
  outputRoot            <- Path.resolve [cwd] "../src/"
  { sources, types }    <- typescript path regex
  let filteredSources   =  Array.filter (\(DeclarationSourceFile { fileName }) -> isNothing $ String.indexOf (String.Pattern "index.d.ts") fileName) sources
  let context           =  { sources : filteredSources, types, typeReplacements, regexps, outputRoot }
  let state             =  { files : mempty, hasRequiredFields : Object.empty, reverseImports : initialImports }
  tuple                 <- Run.runBaseEffect
                            $ State.runState state
                            $ Reader.runReader context write
--  let _ = spy "state" tuple
  pure unit

write :: ReactWriter Unit
write = do
  setup
  { sources } <- Reader.ask
  traverse_ writeSource sources

setup :: ReactWriter Unit
setup = do
  { sources } <- Reader.ask
  traverse_ setHasRequiredFields sources
  where
    setHasRequiredFields src = do 
      let ifaces = interfaces src
      ifaces # traverse_ (\rec -> do
        let optionals = toArrayOf (traversed <<< _PropertySignature <<< _isOptional) rec.typeMembers
        let isRequired = not $ Array.all identity optionals
        state <- State.get 
        State.put state { hasRequiredFields = Object.insert rec.name isRequired state.hasRequiredFields })


writeSource :: DeclarationSourceFile -> ReactWriter Unit
writeSource source @ (DeclarationSourceFile { fileName }) = do
  Run.liftEffect $ log $ "Writing " <> fileName
  setPathAndModuleName source
  importsForSource source
  javascriptExports source
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
        , imports
        , elementsPart
        ]
  Run.liftEffect $ FS.writeTextFile UTF8 psjs.purescript.path body 

basicImports :: String
basicImports = """import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)
"""

initialImports :: Object String
initialImports = Object.empty #
  Object.insert "Object" "Foreign.Object" #
  Object.insert "ReactComponent" "React.Basic" #
  Object.insert "JSX" "React.Basic" #
  Object.insert "element" "React.Basic" #
  Object.insert "CSS" "React.Basic.DOM.Internal" #
  Object.insert "EventHandler" "React.Basic.Events"


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
  let strs = map handleImport $ Object.toUnfoldable psjs.purescript.imports
  let imports = Array.intercalate "\n" $ Array.filter (\i -> i /= "") strs
  pure imports
  where
    handleImport (Tuple moduleName strs) = fold
      [ "import "
      , if moduleName == "React.Basic.MUI." then "React.Basic.MUI.Styles" else if moduleName == "Theme" then "React.Basic.MUI.Styles.CreateMuiTheme" else moduleName
      , if moduleName == "React.Basic" then " (element, ReactComponent, " else " ("
      , Array.intercalate ", " strs 
      , ")"
      ]
     


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
showDeclarationElements (TypeAliasDeclaration rec) = showTypeAliasDeclaration rec
showDeclarationElements elem = pure "" 

showTypeAliasDeclaration :: TypeAliasDeclarationRec -> ReactWriter String
showTypeAliasDeclaration {name, typeParameters, type: _type } = do 
  t <- showTSType _type
  let parameters = Array.intercalate " " $ map showTypeParameter typeParameters
  pure $ fold
    [ "type "
    , name
    , if Array.length typeParameters == 0 then "" else " "
    , parameters
    , " = "
    , t 
    ]

showInterfaceDeclaration :: InterfaceDeclarationRec -> ReactWriter String
showInterfaceDeclaration { name, typeParameters, typeMembers } = do
  let partition = Array.partition isOptional typeMembers 
  let optionalMembers = partition.yes
  let requiredMembers = partition.no
  if (Array.length requiredMembers) == 0
    then Array.intercalate "\n\n" <$> sequence [ optionalType optionalMembers, foreignData, createForeignData ] 
    else Array.intercalate "\n\n" <$> sequence [ requiredType requiredMembers, optionalType optionalMembers, foreignData, createForeignData ] 
  where
    typeParametersStr | Array.length typeParameters > 0 = " " <> (Array.intercalate " " $ map showTypeParameter typeParameters)
    typeParametersStr = ""
    requiredType members = (showMembers members) <#> \body ->  ("type " <> name <> "_required optional" <> typeParametersStr <> " =\n  ( " <> body <> "\n  | optional )")
    optionalType members = (showMembers members) <#> \body ->  ("type " <> name <> "_optional" <> typeParametersStr <> " =\n  ( " <> body <> "\n  )")
    foreignData = pure $ fold [ "foreign import data ", name, " :: Type ", (Array.intercalate " -> " $ map (const "Type") $ fromMaybe [] $ Array.tail typeParameters) ]
    allType members = (showMembers members) <#> \body ->  ("type " <> name <> " " <> typeParametersStr <> " =\n  { " <> body <> "\n  }")
    createForeignData = do
      lower <- lowerCaseFirst name 
      signature  <- unionSignatureFn lower name name
      pure $ fold [ signature, "\n", lower, " = unsafeCoerce" ]


    showMembers :: Array TypeMember -> ReactWriter String
    showMembers members = do 
      strs <- traverse showTypeMember members
      pure $ Array.intercalate "\n  , " $ strs 

showTypeParameter :: TypeParameter -> String
showTypeParameter (TypeParameter param) = String.toLower param

showTypeMember :: TypeMember -> ReactWriter String
showTypeMember typeMember = do
  context <- Reader.ask
  pure $ showTypeMemberWithContext context typeMember

showTypeMemberWithContext :: Context -> TypeMember -> String
showTypeMemberWithContext context (PropertySignature (signature @ { name : Just name })) = do
  let fieldType = showUnfilteredTSType context signature.type
  ((clean context.regexps.cleanFunctionName $ showPropertyName name) <> " :: " <> fieldType)
  where
    clean :: Regex -> String -> String
    clean regex str = case (Regex.match regex str) of 
      Nothing -> "\"" <> str <> "\""
      Just _ -> str 
showTypeMemberWithContext context (PropertySignature signature) = showUnfilteredTSType context signature.type
showTypeMemberWithContext context typeMember = show typeMember


showPropertyName :: PropertyName -> String
showPropertyName (IdentifierName name) = name
showPropertyName (StringLiteral name) = name
showPropertyName (NumericLiteral name) = show (show name)

showVariableDeclarations :: Array VariableDeclaration -> ReactWriter String
showVariableDeclarations = (map $ Array.intercalate "\n\n") <<< traverse showVariableDeclaration 

showVariableDeclaration :: VariableDeclaration -> ReactWriter String
showVariableDeclaration (VariableDeclaration rec) = do
  _type <- showTSType rec.type
  if _type == "JSX"
    then jsxFn rec.name
    else normalFn rec.name _type


showFunctionElement :: FunctionElementRec -> ReactWriter String
showFunctionElement { name : Nothing } = pure ""
showFunctionElement { name : Just name, typeParameters, parameters, returnType } = do
  _type <- showTSType returnType
  if _type == "JSX"
    then jsxFn name
    else normalFn name _type

normalFn :: String -> String -> ReactWriter String
normalFn name _type = do
  lower <- lowerCaseFirst name
  let body = fold [ lower, " = _", name ]
  let foreignData = fold [ "foreign import _", name, " :: ", _type ]
  pure $ fold [ lower , " :: " , _type, "\n", body, "\n", foreignData ]   


jsxFn :: String -> ReactWriter String
jsxFn name = do
  state <- State.get
  let hasRequired = fromMaybe false $ Object.lookup (name <> "Props") state.hasRequiredFields
  lower <- lowerCaseFirst name
  let recordTypePart = if hasRequired then (name <> "_required attrs") else "attrs"
  signature <- unionSignatureFn name (name <> "Props") "JSX"
  let body = fold [ lower, " = element _", name ]
  let foreignData = fold [ "foreign import _", name, " :: forall a. ReactComponent a " ]
  pure $ fold [ signature, "\n", body, "\n", foreignData ]

unionSignatureFn :: String -> String -> String -> ReactWriter String
unionSignatureFn name recordName _type = do
  state <- State.get
  let hasRequired = fromMaybe false $ Object.lookup (recordName) state.hasRequiredFields
  lower <- lowerCaseFirst name
  let recordTypePart = if hasRequired then (recordName <> "_required attrs") else "attrs"
  let signature = fold 
        [ lower
        , "\n  :: ∀ attrs attrs_\n   . Union attrs attrs_ ("
        , recordName 
        , "_optional)\n  => Record ("
        , recordTypePart
        , ")\n  -> "
        , _type
        ]
  pure signature


replaceType :: Context -> String -> String
replaceType { typeReplacements }_type = do
  fromMaybe _type $ Array.foldl replace Nothing typeReplacements
  where
    replace :: Maybe String -> TypeReplacement -> Maybe String 
    replace (Just value) _ = Just value
    replace _ (Exists comp value) | isJust $ String.indexOf (String.Pattern comp) _type = Just value
    replace _ (RegexReplace regex value) | isJust $ Regex.match regex _type = Just value
    replace _ (Exact comp value) | _type == comp = Just value
    replace _ _ = Nothing

showTSType ::  TSType -> ReactWriter String
showTSType tsType = do
  context <- Reader.ask
  pure $ replaceType context $ showUnfilteredTSType context tsType

showUnfilteredTSType :: Context -> TSType -> String
showUnfilteredTSType _ BooleanType = "Boolean"
showUnfilteredTSType _ StringType = "String"
showUnfilteredTSType _ NumberType = "Number"
showUnfilteredTSType _ BigIntType = "Number"
showUnfilteredTSType _ ObjectType = "(Object Foreign)"
showUnfilteredTSType _ VoidType = "Unit"
showUnfilteredTSType _ AnyType = "Foreign"
showUnfilteredTSType _ SymbolType = "Foreign"
showUnfilteredTSType _ UnknownType = "Foreign"
showUnfilteredTSType _ NeverType = "Foreign"
showUnfilteredTSType context (ParenthesizedType t) = "(" <> (showUnfilteredTSType context t) <> ")"
showUnfilteredTSType context (ArrayType t) = "(Array " <> (showUnfilteredTSType context t) <> ")"
showUnfilteredTSType context (TypeReference { name, typeArguments }) | Array.length typeArguments > 0 && replaceType context (showEntityName name) == showEntityName name =  showEntityName name <> " " <> (Array.intercalate " " $ map (showUnfilteredTSType context) typeArguments)
showUnfilteredTSType context (TypeReference { name, typeArguments }) | replaceType context (showEntityName name) == showEntityName name =  showEntityName name <> " " <> (Array.intercalate " " $ map (showUnfilteredTSType context) typeArguments)
showUnfilteredTSType context (TypeReference { name, typeArguments }) = replaceType context $ showEntityName name
--    showUnfilteredTSType (FunctionType { typeParameters, parameters, returnType }) | Array.length typeParameters == 0 = (Array.intercalate " -> " $ map (showTypeMemberAsFunctionType replacements) parameters) <> " -> " <> (showTSType  replacements returnType)
--    showUnfilteredTSType (FunctionType { typeParameters, parameters, returnType }) = "forall " <> (Array.intercalate " " $ map showTypeParameter typeParameters) <> ". " <> (Array.intercalate " -> " $ map (showTypeMemberAsFunctionType replacements ) parameters) <> " -> " <> (showTSType replacements returnType)
showUnfilteredTSType context (UnionType types) = "Foreign"
showUnfilteredTSType context (IntersectionType types) = "Foreign"
showUnfilteredTSType _ (LiteralType (LiteralStringValue value)) = "String"
showUnfilteredTSType _ (LiteralType (LiteralNumericValue value)) = "Number"
showUnfilteredTSType _ (LiteralType (LiteralBigIntValue value)) = "Number"
showUnfilteredTSType _ (LiteralType (LiteralBooleanValue value)) = "Boolean"
showUnfilteredTSType context (TypeLiteral members) = "{ " <> (Array.intercalate ", " $ map (showTypeMemberWithContext context) members) <> " }"
showUnfilteredTSType _ (TypeQuery entityName) = showEntityName entityName
showUnfilteredTSType _ t = "Foreign"

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
setPathAndModuleName (DeclarationSourceFile { fileName, elements }) = do
  { regexps, outputRoot } <- Reader.ask
  let match       =  Regex.match regexps.codegenPath fileName 
  let splitter    =  String.split (String.Pattern "/")
  tokens          <- Array.nubEq <$> (traverse upperCaseFirst $ maybe [] splitter $ preview (_Just <<< ix 1 <<< _Just) match)
  mkDir outputRoot tokens
  let pathPrefix  =  Array.intercalate "/" tokens
  jsPath          <- Run.liftEffect $ Path.resolve [outputRoot] $ pathPrefix <> ".js"
  psPath          <- Run.liftEffect $ Path.resolve [outputRoot] $ pathPrefix <> ".purs"
  let parentModuleName = "React.Basic.MUI." <> (maybe "" (Array.intercalate "." ) $ Array.init tokens)
  let name = fromMaybe "" $ if Array.length tokens > 1
                              then Array.head =<< Array.tail tokens
                              else Array.head tokens
  let moduleName  =  "React.Basic.MUI." <> (Array.intercalate "." tokens) 
  psjs          <- getPSJS fileName 
  setPSJS (psjs { javascript = psjs.javascript { path = jsPath }, purescript = psjs.purescript { path = psPath, name = name, moduleName = moduleName, parentModuleName = parentModuleName } })
  state <- State.get
  traverse_ (elementSetReverseImport moduleName) elements
    where
      mkDir :: FilePath -> Array String -> ReactWriter Unit
      mkDir _ tokens | Array.length tokens <= 1 = pure unit
      mkDir outputRoot tokens = do
        path    <- Run.liftEffect $ Path.resolve [outputRoot] $ maybe outputRoot (Array.intercalate "/") $ Array.init tokens
        exists  <- Run.liftEffect $ FS.exists path 
        if not exists then Run.liftEffect $ FS.mkdir path else pure unit

      elementSetReverseImport :: String -> DeclarationElements -> ReactWriter Unit
      elementSetReverseImport moduleName (InterfaceDeclaration { name }) = setReverseImport name moduleName
      elementSetReverseImport moduleName (TypeAliasDeclaration { name }) = setReverseImport name moduleName
      elementSetReverseImport moduleName (FunctionElement { name: Just name }) = setReverseImport name moduleName
      elementSetReverseImport moduleName (VariableStatement declarations) = traverse_ (moduleName # flip setReverseImport) $ 
        map (\(VariableDeclaration { name }) -> name) declarations
      elementSetReverseImport _ _ = pure unit


      setReverseImport :: String -> String -> ReactWriter Unit
      setReverseImport name moduleName = do
        State.modify \state -> state { reverseImports = Object.insert name moduleName state.reverseImports }
 

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
  context             <- Reader.ask
  let typeReferences  = join $ map handleElement elements
  let types           = Array.nubEq $ map (replaceType context) $ extractTypeNames typeReferences
  { reverseImports }  <- State.get
  psjs <- getPSJS fileName
  let importTuples = Array.mapMaybe (\t -> (Object.lookup t reverseImports) <#> \imps -> Tuple imps t ) types
  let imports = Array.foldl (\obj (Tuple moduleName thing) -> 
      if moduleName /= psjs.purescript.moduleName
        then case (Object.lookup moduleName obj) of
            (Just array) -> Object.insert moduleName (Array.cons thing array) obj
            Nothing -> Object.insert moduleName [ thing ] obj
        else obj
      ) Object.empty importTuples

  setPSJS psjs { purescript { imports = imports } }
  where
    extractNames :: Array String
    extractNames = join $ map getName elements 
      where
        getName :: DeclarationElements -> Array String
        getName (InterfaceDeclaration rec) = [ rec.name ]
        getName (TypeAliasDeclaration rec) = [ rec.name ]
        getName (FunctionElement rec) = [ fromMaybe "" rec.name ]
        getName (VariableStatement declarations) = join $ declarations <#> (\(VariableDeclaration { name, fullyQualifiedName }) -> [ name ])
        getName _ = []

    extractTypeNames :: Array TypeReferenceRec -> Array String
    extractTypeNames = map extractTypeName 

    extractTypeName :: TypeReferenceRec -> String 
--    extractTypeName { fullyQualifiedName: (Just fullyQualifiedName) } | (fullyQualifiedName /= "__type" && fullyQualifiedName /= "__type.bivarianceHack") = fullyQualifiedName
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


 