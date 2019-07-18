module Bin.Codegen.Icon where

import Prelude

import Data.Array (elem, fromFoldable) as Array
import Data.Char.Unicode (toLower) as Unicode
import Data.Foldable (for_)
import Data.List (List)
import Data.List (filter, length) as List
import Data.Maybe (Maybe(..), fromMaybe, optional)
import Data.String (joinWith) as String
import Data.String.CodeUnits (singleton, uncons) as CodeUnits
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Path (concat) as Path
import Node.Process (stderr)
import Node.Stream (writeString)
import Options.Applicative (Parser, argument, execParser, fullDesc, help, helper, info, long, many, metavar, str, strOption, (<**>))

-- | List of all "material-ui/icons/" submodule names
foreign import icons ∷ Array String

ffiExportName ∷ String → String
ffiExportName icon = "_" <> icon

iconName ∷ String → String
iconName "Class" = "class_"
iconName icon = case CodeUnits.uncons icon of
  Just { head, tail } → CodeUnits.singleton (Unicode.toLower head) <> tail
  Nothing → icon

jsModule ∷ String → String
jsModule icon =
  let
    export
      = "exports." <> ffiExportName icon
      <> " = "
      <> "require('@material-ui/icons/" <> icon <> "').default;"
  in
    String.joinWith "\n"
        [ "/* global exports, require */"
        , ""
        , export
        ]

psModule ∷ String → String
psModule icon =
  let
    i = iconName icon
    e = ffiExportName icon
    lines =
      [ "module MUI.Icons." <> icon <> " where"
      , ""
      , "import MUI.Core.SvgIcon (SvgIconProps)"
      , "import Prim.Row (class Union)"
      , "import React.Basic (JSX, ReactComponent, element)"
      , "import React.Basic.DOM (Props_svg)"
      , ""
      , "foreign import " <> e <> " ∷ ∀ a. ReactComponent a"
      , ""
      , i <> "_component ∷ ∀ componentProps props props_"
      , "  . Union props props_ (SvgIconProps componentProps)"
      , "  ⇒ Record props"
      , "  → JSX"
      , i <> "_component = element " <> e
      , ""
      , i <> " ∷ ∀ props props_"
      , "  . Union props props_ (SvgIconProps Props_svg)"
      , "  ⇒ Record props"
      , "  → JSX"
      , i <> " = element " <> e
      ]
  in
    String.joinWith "\n" lines

newtype Options = Options
  { dir ∷ Maybe String
  , icons ∷ List String
  }

options ∷ Parser Options
options = map Options $ { dir: _, icons: _ }
  <$> optional (strOption (long "dir" <> metavar "OUTPUT_DIR" <> help "Icons modules will go there"))
  <*> many (argument str (metavar "ICONS..."))

logStderr ∷ String → Effect Unit
logStderr msg = void $ writeString stderr UTF8 msg mempty

main ∷ Effect Unit
main = do
  Options opts ← execParser (info (options <**> helper) fullDesc)
  let
    outputDir = fromMaybe "" opts.dir
    notFound = List.filter (not <<< flip Array.elem icons) opts.icons
  if List.length notFound > 0
    then
      logStderr $ "Some icons have not been found by our simple mui-icons processor: "
        <> String.joinWith "," (Array.fromFoldable notFound)
    else
      for_ opts.icons \icon → launchAff_ do
        let
          jsPath = Path.concat [ outputDir, icon <> ".js" ]
          psPath = Path.concat [ outputDir, icon <> ".purs" ]

        writeTextFile UTF8 jsPath (jsModule icon)
        writeTextFile UTF8 psPath (psModule icon)


