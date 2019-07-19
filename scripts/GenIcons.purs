module Scripts.GenIcons where

import Prelude

import Control.Alternative ((<|>))
import Data.Array (elem, fromFoldable) as Array
import Data.Char.Unicode (toLower) as Unicode
import Data.Foldable (for_)
import Data.List (List)
import Data.List (filter, length) as List
import Data.Maybe (Maybe(..), fromMaybe, optional)
import Data.String (joinWith) as String
import Data.String.CodeUnits (singleton, uncons) as CodeUnits
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import Node.Path (concat) as Path
import Node.Process (stderr)
import Node.Stream (writeString)
import Options.Applicative (Parser, execParser, flag', fullDesc, help, helper, info, long, many, metavar, strOption, (<**>))

-- | List of all "material-ui/icons/" submodule names
foreign import allIcons ∷ Array String

foreign import data BagPipe ∷ Type
foreign import bagpipe ∷ Int → Effect BagPipe
foreign import push ∷ BagPipe → Effect Unit → Effect Unit

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

data Icons
  = Icons (List String)
  | All

newtype Options = Options
  { dir ∷ Maybe String
  , icons ∷ Icons
  }

options ∷ Parser Options
options = map Options $ { dir: _, icons: _ }
  <$> optional (strOption (long "dir" <> metavar "OUTPUT_DIR" <> help "Icons modules will go there"))
  <*> icons
  where
    icons =
      (Icons <$> (many (strOption (long "icon"))))
      <|> flag' All ( long "all" <> help "Generate all icons" )

logStderr ∷ String → Effect Unit
logStderr msg = void $ writeString stderr UTF8 msg mempty

main ∷ Effect Unit
main = do
  Options opts ← execParser (info (options <**> helper) fullDesc)
  let
    outputDir = fromMaybe "" opts.dir
  case opts.icons of
    All → generate outputDir allIcons
    Icons icons → do
      let
        notFound = List.filter (not <<< flip Array.elem allIcons) icons
      if List.length notFound > 0
        then
          logStderr $ "Some icons have not been found by our simple mui-icons processor: "
            <> String.joinWith "," (Array.fromFoldable notFound)
        else
          generate outputDir (Array.fromFoldable icons)
  where
    generate outputDir icons = for_ icons \icon → do
        b ← bagpipe 300
        let
          jsPath = Path.concat [ outputDir, icon <> ".js" ]
          psPath = Path.concat [ outputDir, icon <> ".purs" ]

        push b $ do
          writeTextFile UTF8 jsPath (jsModule icon)
          writeTextFile UTF8 psPath (psModule icon)


