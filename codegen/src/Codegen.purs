module Codegen where

import Prelude

import Codegen.AST (Ident(..), Module(..), ModuleName(..)) as AST
import Codegen.AST.Printers (printModule)
import Codegen.AST.Sugar.Type (constructor) as Type
import Codegen.Model (Component, Icon(..), ModulePath(..), iconFullPath, psImportPath)
import Codegen.Model (ModulePath(..), componentFullPath, componentName, iconFullPath, iconName) as Model
import Codegen.TS (M) as TS
import Codegen.TS.MUI (componentAST) as TS.MUI
import Codegen.TS.MUI (componentConstructorsAST, foreignReactComponentDecl)
import Codegen.TS.Module (declarations)
import Data.Foldable (intercalate)
import Data.List (List(..), fromFoldable) as List
import Data.Maybe (Maybe(..))
import Data.Moldy (Moldy(..))
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

psModuleFile :: ModulePath -> File
psModuleFile (Path str next) = Directory str $ psModuleFile next
psModuleFile (Name name) = File $ name <> ".purs"

jsModuleFile :: ModulePath -> File
jsModuleFile (Path str next) = Directory str $ jsModuleFile next
jsModuleFile (Name name) = File $ name <> ".js"

componentPSFile :: Component -> File
componentPSFile = psModuleFile <<< Model.componentFullPath

componentJSFile :: Component -> File
componentJSFile = jsModuleFile <<< Model.componentFullPath

iconPSFile :: Icon -> File
iconPSFile = psModuleFile <<< Model.iconFullPath

iconJSFile :: Icon -> File
iconJSFile = jsModuleFile <<< Model.iconFullPath

genComponentPS :: Component -> M String
genComponentPS c =
  TS.MUI.componentAST c <#> printModule

-- | TODO: FFI generation should be done in the same place as PS codegen
genComponentJS :: Component -> String
genComponentJS c@{ modulePath }
  = "exports._" <> Model.componentName c
  <> " = require(\"@material-ui/core/" <> (jsPath modulePath) <> "\").default;"
  where
    jsPath (Path str next) = (String.toLower str) <> "/" <> (jsPath next)
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

component :: Component -> TS.M { ps :: String, js :: String }
component c =
  { ps: _, js: genComponentJS c} <$> (genComponentPS c)

foreign import icons :: Array Icon

icon :: Icon -> { ps :: String, js :: String }
icon i = { ps, js }
  where
    iconName = Model.iconName i

    svgIconProps = Type.constructor "MUI.Core.SvgIcon.SvgIconProps"
    props_svg = Type.constructor "React.Basic.DOM.Props_svg"
    declarations = componentConstructorsAST svgIconProps (Just props_svg) iconName

    -- | TODO: Same as above - this should be intergrated into module codegen
    rc@{ ident: AST.Ident ffiName } = foreignReactComponentDecl iconName
    js = "exports." <> ffiName <> " = " <> "require('@material-ui/icons/" <> iconName <> "').default;"

    ps = printModule $ AST.Module
      { declarations
      , moduleName: AST.ModuleName $ psImportPath (iconFullPath i)
      }
