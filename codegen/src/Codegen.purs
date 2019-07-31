module Codegen where

import Prelude

import Codegen.Model (Component, Module(..), PropType(..), Variant(..), VariantProp(..), classKeyGenericName, classKeyJSSName, classKeyName, classKeyRowJSSName, classKeyRowName, propsName, propsRowName)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Foreign.Object (Object)
import Foreign.Object as Object
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path (FilePath)

data Codegen = Codegen File String

data File
  = File String
  | Directory String File

standardModules :: Object (Array String)
standardModules = Object.fromFoldable
  [ Tuple "MUI.Core" ["JSS"]
  , Tuple "Prim.Row" ["class Union"]
  , Tuple "React.Basic" ["JSX", "ReactComponent", "element"]
  , Tuple "Unsafe.Coerce" ["unsafeCoerce"]
  ]

variantModules :: Object (Array String)
variantModules = Object.fromFoldable
  [ Tuple "Prelude" []
  , Tuple "Unsafe.Coerce" ["unsafeCoerce"]
  ]

toObject :: Array (Tuple String (Array String)) -> Object (Array String)
toObject = Array.foldl (\obj tuple -> obj <> (Object.fromFoldable [ tuple ])) Object.empty

genImports :: Component -> String
genImports { props, inherits, variants } = do
  let propsObj = toObject $ Array.catMaybes $ ((Object.toUnfoldable props <#> snd) <#> extractImports) >>= identity
      inheritsObj = toObject $ Array.catMaybes $ extractImports inherits
      modules = propsObj <> inheritsObj
      allModules = modules <> standardModules <> (if Array.length (simpleVariants variants) > 0 then variantModules else Object.empty)
      sorted = (Object.toUnfoldable allModules) # (Array.sortWith fst)
      filtered = Array.filter (\(Tuple moduleName _) -> moduleName /= "Prelude") sorted
      imports = Array.intercalate "\n" 
          $ map (\(Tuple moduleName types) -> 
            Array.fold ["import ", moduleName, " (", (Array.intercalate ", " (Array.nub types)), ")" ]) 
          filtered 
  if Array.elem "Prelude" $ map fst sorted
    then "import Prelude\n\n" <> imports
    else imports

extractImports :: PropType -> Array (Maybe (Tuple String (Array String)))
extractImports (ArrayProp prop) = extractImports prop
extractImports (ImportProp moduleName typeName) = [ Just (Tuple moduleName [ typeName ]) ]
extractImports (ParensList propType1 propType2) = (extractImports propType1) <> (extractImports propType2)
extractImports (PropList propType1 propType2) = (extractImports propType1) <> (extractImports propType2)
extractImports (ReactComponent propType) = extractImports propType
extractImports (RecordType propType) = extractImports propType
extractImports UnitProp = [ Just (Tuple "Prelude" [ "Unit" ]) ]
extractImports StringProp = []
extractImports BooleanProp = []
extractImports NumberProp = []
extractImports (TypeVariable _) = []
extractImports (Local _) = []
extractImports Done = []

pureScriptFile :: Module -> File
pureScriptFile (Path str next) = Directory str $ pureScriptFile next
pureScriptFile (Name name) = File $ name <> ".purs"

javaScriptFile :: Module -> File
javaScriptFile (Path str next) = Directory str $ javaScriptFile next
javaScriptFile (Name name) = File $ name <> ".js"


genModuleName :: Module -> String
genModuleName m = "module MUI." <> go m 
  where
    go :: Module -> String
    go (Path str next) = str <> "." <> go next
    go (Name name) = name <> " where"

genPureScript :: Component -> Array Codegen
genPureScript component = do
  let moduleName = genModuleName component.moduleName
      imports = genImports component
      props = genProps component
      variants = genSimpleVariants $ simpleVariants component.variants
      classKey = genClassKey component
      classKeyFn = genClassKeyFn component
      classKeyJSSFn = genClassKeyJSSFn component
      fn = genFn component
      componentFn = genComponentFn component
      foreignData = genForeign component
      code = Array.intercalate "\n\n" 
        [ moduleName
        , imports
        , props
        , variants
        , classKey
        , classKeyFn
        , classKeyJSSFn
        , fn
        , componentFn
        , foreignData
        ]
      file = pureScriptFile component.moduleName
  [ Codegen file code ] <> (Array.mapMaybe genVariantPureScript component.variants)

genVariantPureScript :: Variant -> Maybe Codegen
genVariantPureScript (SimpleVariant _ _) = Nothing
genVariantPureScript (ModuleVariant _moduleName name values) = do
  let moduleName = genModuleName _moduleName 
  let imports = "import Prelude\n\nimport Unsafe.Coerce (unsafeCoerce)"
  let body = genSimpleVariant (Tuple name values)
  let code = Array.intercalate "\n\n" [ moduleName, imports, body ]
  Just $ Codegen (pureScriptFile _moduleName) code

genVariantJavaScript :: Variant -> Maybe Codegen
genVariantJavaScript (SimpleVariant _ _) = Nothing
genVariantJavaScript (ModuleVariant moduleName name _) = do
  let file = javaScriptFile moduleName
      code = genVariantJSLine name
  Just $ Codegen file code

simpleVariants :: Array Variant -> Array (Tuple String (Array VariantProp))
simpleVariants = Array.mapMaybe extract
  where
    extract (SimpleVariant name values ) = Just $ Tuple name values
    extract _ = Nothing

genJavaScript :: Component -> Array Codegen 
genJavaScript { name, moduleName, variants } = do
  let importLine = "exports._" <> name <> " = require(\"@material-ui/" <> (jsPath moduleName) <> "\").default;"
      code = if Array.length (simpleVariants variants) == 0 
              then importLine 
              else importLine <> "\n" <> 
                (Array.intercalate "\n" $ map genVariantJS variants)
      file = javaScriptFile moduleName
  [ Codegen file code ] <> (Array.mapMaybe genVariantJavaScript variants)
  where
    jsPath (Path str next) = (String.toLower str) <> "/" <> (jsPath next)
    jsPath (Name n) = n

    genVariantJS (SimpleVariant varName _) = genVariantJSLine varName
    genVariantJS (ModuleVariant _ varName _) = genVariantJSLine varName

genVariantJSLine :: String -> String
genVariantJSLine varName = do 
  let eqLine = "exports._eq" <> varName <> " = function(left){ return function(right){ return left === right }};"
      ordLine = "exports._ord" <> varName <> " = function(left){ return function(right){ return (left === right) ? 0 : (left > right) ? 1 : -1 }};"
  eqLine <> "\n" <> ordLine

toType :: PropType -> String
toType StringProp = "String"
toType BooleanProp = "Boolean"
toType NumberProp = "Number"
toType UnitProp = "Unit"
toType (ArrayProp propType) = "(Array " <> (toType propType) <> ")"
toType (ImportProp _ typeName) = typeName
toType (ReactComponent propType) = "ReactComponent { | " <> (toType propType) <> " }"
toType (PropList propTypeL propTypeR)  = (toType propTypeL) <> " " <> (toType propTypeR)
toType (ParensList propTypeL propTypeR) = "(" <> (toType propTypeL) <> " " <> (toType propTypeR) <> ")"
toType (Local typeName) = typeName
toType (TypeVariable variableName) = variableName
toType (RecordType propType) = "{ | " <> (toType propType) <> " }"
toType Done = ""

genProps :: Component -> String
genProps { name, props, componentTypeVariable, additionalTypeVariables } = do
  typeDecl <> body <> propsBottom <> foreignData
  where
    typeDecl = "type " <> (propsRowName name) <> " " <> typeVariables <> " = \n  ( "
    typeVariables = Array.intercalate " " $ fromMaybe additionalTypeVariables $ (componentTypeVariable <#> (flip Array.cons additionalTypeVariables))
    body = Array.intercalate "\n  , " 
              $ Array.sortWith (String.replaceAll (String.Pattern "\"") (String.Replacement "") <<< String.toLower) 
              $ Object.foldMap (\fieldName propType fields -> 
                  Array.cons (Array.fold [ (handleFieldName fieldName), " :: ", toType propType ]) fields
                ) props []
    propsBottom = fromMaybe "\n  )" (componentTypeVariable <#> \str -> "\n  | " <> str <> "\n  )")
    handleFieldName fieldName = 
      if isJust $ Regex.match (unsafeRegex "^[A-Z]" noFlags) fieldName
        then "\"" <> fieldName <> "\""
        else fieldName
    foreignData = "\n\nforeign import data " <> (propsName name) <> " :: Type"

genSimpleVariants :: Array (Tuple String (Array VariantProp)) -> String
genSimpleVariants = Array.intercalate "\n" <<< map genSimpleVariant

genSimpleVariant :: Tuple String (Array VariantProp) -> String
genSimpleVariant (Tuple name values) = do
    Array.intercalate "\n\n" 
        $ Array.cons (Array.intercalate "\n" [ foreignData, genVariantEqTypeClass, genVariantOrdTypeClass ])
        $ map genVariantFn values
    where
      genVariantFn :: VariantProp -> String 
      genVariantFn (StringVariant fnName) = do
        let typeDecl = fnName <> " :: " <> name
        let body = fnName <> " = unsafeCoerce " <> (show fnName)
        typeDecl <> "\n" <> body
      genVariantFn (StringNameVariant fnName valueName) = do
        let typeDecl = fnName <> " :: " <> name
        let body = fnName <> " = unsafeCoerce " <> (show valueName)
        typeDecl <> "\n" <> body
      genVariantFn (NumberVariant fnName number) = do
        let typeDecl = fnName <> " :: " <> name
        let body = fnName <> " = unsafeCoerce " <> (show number)
        typeDecl <> "\n" <> body
      genVariantFn BooleanVariant = do
        let typeDecl = "boolean :: Boolean -> " <> name
        let body = "boolean value = unsafeCoerce value"
        typeDecl <> "\n" <> body

      genVariantEqTypeClass =
        "instance eq" <> name <> " :: Eq " <> name <> " where" <> " eq left right = _eq" <> name <> " left right"
      genVariantOrdTypeClass =
        "instance ord" <> name <> " :: Ord " <> name <> " where" <> " compare left right = compare (" <> "_ord" <> name <> " left right) (" <> "_ord" <> name <> " right left)"
      foreignData = Array.intercalate "\n"
        [ "foreign import data " <> name <> " :: Type"
        , "foreign import _eq" <> name <> " :: " <> name <> " -> " <> name <> " -> Boolean"
        , "foreign import _ord" <> name <> " :: " <> name <> " -> " <> name <> " -> Int"
        ]





genClassKey :: Component -> String
genClassKey { name, classKey } = do
  Array.intercalate "\n" [ genericType, stringType, jssType, foreignClassKey, foreignClassKeyJSS ]
  where
    stringType = "type " <> (classKeyRowName name) <> " = " <> (classKeyGenericName name) <> " String"
    jssType = "type " <> (classKeyRowJSSName name) <> " = " <> (classKeyGenericName name) <> " JSS"
    foreignClassKey = "foreign import data " <> (classKeyName name) <> " :: Type"
    foreignClassKeyJSS = "foreign import data " <> (classKeyJSSName name) <> " :: Type"
    genericType = "type " <> (classKeyGenericName name) <> " a =\n  ( " <> 
        (Array.intercalate "\n  , " $ map (\c -> c <> " :: a ") classKey) <>
        "\n  )"

genUnionFunction :: String -> String -> String -> String -> String -> String
genUnionFunction name rowType typeVariables returnType body = do 
  Array.intercalate "\n" [ declLine, unionLine, recordLine, returnLine, bodyLine ]
  where
    lowerName = Regex.replace' (unsafeRegex "^\\w" noFlags) (\match _ -> String.toLower match) name
    declLine = lowerName <> " :: ∀ " <> typeVariables <> " given required"
    unionLine = "  .  Union given required (" <> rowType <> " " <> typeVariables <> ")"
    recordLine = "  => Record given" 
    returnLine = "  -> " <> returnType
    bodyLine = lowerName <> " = " <> body

genClassKeyFn :: Component -> String
genClassKeyFn component = do
  let name = classKeyName component.name
      rowType = classKeyRowName component.name
      typeVariables = ""
      returnType = classKeyName component.name
      body = "unsafeCoerce"
  genUnionFunction name rowType typeVariables returnType body

genClassKeyJSSFn :: Component -> String
genClassKeyJSSFn component = do
  let name = classKeyJSSName component.name
      rowType = classKeyRowJSSName component.name
      typeVariables = ""
      returnType = classKeyJSSName component.name
      body = "unsafeCoerce"
  genUnionFunction name rowType typeVariables returnType body

genComponentFn :: Component -> String
genComponentFn component = do
  let name = component.name <> "_component"
      rowType = (propsRowName component.name)
      typeVariables = Array.intercalate " " 
                        $ fromMaybe component.additionalTypeVariables 
                        $ component.componentTypeVariable <#> 
                            (flip Array.cons component.additionalTypeVariables)
      returnType = "JSX"
      body = "element _" <> component.name
  genUnionFunction name rowType typeVariables returnType body

genFn :: Component -> String
genFn component = do
  let name = component.name
      rowType = (propsRowName component.name) <> " " <> toType component.inherits
      typeVariables = Array.intercalate " " component.additionalTypeVariables
      returnType = "JSX"
      body = "element _" <> name
  genUnionFunction name rowType typeVariables returnType body

genForeign :: Component -> String
genForeign { name } = "foreign import _" <> name <> " :: ∀ a. ReactComponent a"

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


codegen :: Component -> Array Codegen
codegen component =
  (genPureScript component) <> 
  (genJavaScript component)


