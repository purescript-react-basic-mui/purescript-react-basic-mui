module Bin.Codegen.Icons where

import Prelude

import Data.Char.Unicode (toLower) as Unicode
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.String (joinWith) as String
import Data.String.CodeUnits (singleton, uncons) as CodeUnits
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Node.Path (concat) as Path

-- | List of all "material-ui/icons/" submodule names
foreign import icons ∷ Array String

ffiExportName ∷ String → String
ffiExportName icon = "_" <> icon

iconName :: String -> String
iconName "Class" = "class_"
iconName icon = case CodeUnits.uncons icon of
  Just { head, tail } → CodeUnits.singleton (Unicode.toLower head) <> tail
  Nothing → icon

jsModule ∷ String → String
jsModule icon =
  let
    export
      = "exports." <> ffiExportName icon
      <> "="
      <> "require('@material-ui/icons/" <> icon <> "');"
  in
    String.joinWith "\n"
        [ "/* global exports, require */"
        , ""
        , export
        ]

psModule :: String -> String
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
      , "foreign import " <> e <> " :: ∀ a. ReactComponent a"
      , ""
      , i <> "_component :: ∀ componentProps props props_"
      , "  . Union props props_ (SvgIconProps componentProps)"
      , "  => Record props"
      , "  -> JSX"
      , i <> "_component = element " <> e
      , ""
      , i <> " :: ∀ props props_"
      , "  . Union props props_ (SvgIconProps Props_svg)"
      , "  => Record props"
      , "  -> JSX"
      , i <> " = element " <> e
      ]
  in
    String.joinWith "\n" lines


main ∷ Effect Unit
main = launchAff_ $ for_ icons \icon → do
  let
    jsPath = Path.concat [ "./src/MUI/Icons/", icon <> ".js" ]
    psPath = Path.concat [ "./src/MUI/Icons/", icon <> ".purs" ]

  writeTextFile UTF8 jsPath (jsModule icon)
  writeTextFile UTF8 psPath (psModule icon)

