module Main where

import Prelude

import Codegen (codegen)
import Codegen.AST (Ident(..))
import Codegen.AST.Sugar.Type (app, constructor, name, record, row) as Type
import Codegen.Model (ModulePath(..), arrayJSX, jsx, reactComponentApply)
import Data.Either (Either(..))
import Data.Map (fromFoldable) as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)

main :: Effect Unit
main = do
  let
    badge =
      let
        var = Ident "componentProps"
        component = reactComponentApply [ Type.record <<< Type.row mempty $ Just $ Left var ]
        baseProps = Map.fromFoldable
          [ Tuple "badgeContent" jsx
          , Tuple "children" arrayJSX
          , Tuple "component" component
          ]
        base = Type.row baseProps (Just (Left var))
      in
        { extraCode: Nothing
        , inherits: Nothing
        , name: "Badge"
        , modulePath: Path "MUI" (Path "Core" (Name "Badge"))
        , propsType:
          { base: Just base
          , generate: ["anchorOrigin", "classes", "color", "invisible", "max", "showZero", "variant"]
          , vars: [ var ]
          }
        }
    appBar =
      let
        var = Ident "componentProps"
        baseProps = Map.fromFoldable [ Tuple "children" arrayJSX ]
        base = Type.row baseProps (Just (Left var))
      in
        { extraCode: Nothing
        , inherits: Just $ Type.app
            (Type.constructor "MUI.Core.Paper.PaperProps")
            [Type.constructor "React.Basic.Props_div"]
        , name: "AppBar"
        , modulePath: Path "MUI" (Path "Core" (Name "AppBar"))
        , propsType:
          { base: Just base
          , generate: ["classes", "color", "position"]
          , vars: [ var ]
          }
        }

  codegen appBar
