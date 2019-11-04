module Main where

import Prelude

import Codegen (Codegen(..), componentJSFile, componentPSFile, iconJSFile, iconPSFile, icons)
import Codegen (component, icon, write) as Codegen
import Codegen.AST (Ident(..), ModuleName(..), TypeF(..), TypeName(..))
import Codegen.AST.Sugar (declType)
import Codegen.AST.Sugar.Type (app, constructor, record, row) as Type
import Codegen.Model (Component, Icon, ModulePath(..), arrayJSX, componentFullPath, eventHandler, iconName, jsx, psImportPath, reactComponentApply)
import Codegen.Model (componentName) as Model
import Codegen.TS.MUI (componentProps) as TS.MUI
import Codegen.TS.MUI (propsTypeName)
import Control.Alt ((<|>))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (cons, null, sort) as Array
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (for_, intercalate)
import Data.Functor.Mu (roll)
import Data.List (sort) as List
import Data.Map (filterWithKey, fromFoldable, keys, lookup, toUnfoldable) as Map
import Data.Map.Internal (keys) as Map.Internal
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap, wrap)
import Data.Set (difference, member, unions) as Set
import Data.String (Pattern(..), split)
import Data.String (null) as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Matryoshka (cata)
import Node.Path (FilePath)
import Options.Applicative (Parser, ReadM, command, eitherReader, execParser, flag', fullDesc, help, helper, info, long, metavar, option, progDesc, readerError, short, strOption, subparser, value, (<**>))
import Options.Applicative.Types (readerAsk)
import ReadDTS.Instantiation.Pretty (pprintTypeName)

components :: Array Component
components =
  let
    children = Tuple "children" arrayJSX

    foreignType = Type.constructor "Foreign.Foreign"
    -- | variable used For example:
    -- | type AppBarPropsOptions componentProps = (... | componentProps)
    componentPropsIdent = Ident "componentProps"

    component = Tuple "component" $ reactComponentApply
        [ Type.record <<< Type.row mempty $ Just $ Left componentPropsIdent ]

    basePropsRow extraVars props =
      { row: Type.row props (Just (Left componentPropsIdent))
      , vars: [ componentPropsIdent ] <> extraVars
      }

    emptyBase = basePropsRow [] mempty

    simpleComponent { inherits, name, propsType } =
      { extraDeclarations: []
      , inherits: inherits
      -- , name
      , modulePath: (Name name)
      , propsType
      , tsc: { strictNullChecks: false }
      }

    touchRippleType =
      { path: (Path "ButtonBase" (Name "TouchRipple"))
      , name: "TouchRipple"
      }

    appBar = simpleComponent
      { inherits: Just $ Type.app
          (Type.constructor "MUI.Core.Paper.PaperProps")
          [Type.constructor "React.Basic.DOM.Props_div"]
      , name: "AppBar"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children ]
        , generate: ["classes", "color", "position"]
        }
      }
    badge =
      let
        base = basePropsRow [] $ Map.fromFoldable
          [ Tuple "badgeContent" jsx
          , children
          , component
          ]
      in simpleComponent
        { inherits: Nothing
        , name: "Badge"
        , propsType:
          { base
          , generate:
            [ "anchorOrigin", "classes", "color"
            , "invisible", "max", "showZero", "variant"
            ]
          }
        }
    -- | `children` and `component` are taken from `buttonBase`
    button = simpleComponent
      { inherits: Just $ Type.app
        (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions")
        [Type.constructor "React.Basic.DOM.Props_button"]
      , name: "Button"
      , propsType:
        { base: emptyBase
        , generate:
          [ "classes", "color", "disabled", "disableFocusRipple"
          , "disableRipple", "fullWidth", "href", "size", "variant"
          ]
        }
      }
    buttonBase =
      let
        base = basePropsRow [] $ Map.fromFoldable
          [ Tuple "action" foreignType
          , Tuple "buttonRef" foreignType
          , children
          , component
          , Tuple "onFocusVisible" eventHandler
          -- | XXX: Provide some sugar for generating relative imports
          -- | between components
          , Tuple "TouchRippleProps" $ roll $ TypeConstructor
            { moduleName: Just $ ModuleName (psImportPath (componentFullPath touchRipple))
            , name: TypeName $ (propsTypeName touchRippleType.name)
            }
          ]
        buttonBaseActions = declType (TypeName "ButtonBaseActions") [] foreignType
        buttonBaseTypeProps = declType (TypeName "ButtonBaseTypeProp") [] foreignType
      in
        { extraDeclarations:
          [ buttonBaseActions.declaration
          , buttonBaseTypeProps.declaration
          ]
        , inherits: Just $ Type.app
          (Type.constructor "ButtonBasePropsOptions")
          [Type.constructor "React.Basic.DOM.Props_button"]
        , modulePath: Name "ButtonBase"
        , propsType:
          { base
          , generate:
            [ "centerRipple", "classes", "color", "disabled", "disableFocusRipple"
            , "disableRipple", "focusRipple", "focustVisibleClassName"
            , "fullWidth", "href", "size", "type", "variant"
            ]
          }
        , tsc: { strictNullChecks: false }
        }
    fab = simpleComponent
      { inherits: Just $ Type.app
          (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions")
          [Type.constructor "React.Basic.DOM.Props_button"]
      , name: "Fab"
      , propsType:
        { base: emptyBase
        , generate:
          ["classes", "color", "disabled", "disableFocusRipple"
          , "href", "size", "variant"
          ]
        }
      }
    gridList =
      let
        base = basePropsRow [] $ Map.fromFoldable
          [ children
          , component
          ]
        in simpleComponent
          { inherits: Nothing
          , name: "GridList"
          , propsType:
            { base
            , generate: [ "cellHeight", "classes", "cols", "spacing" ]
            }
          }
    gridListTile =
      let
        base = basePropsRow [] $ Map.fromFoldable
          [ children
          , component
          ]
        in simpleComponent
          { inherits: Nothing
          , name: "GridListTile"
          , propsType:
            { base
            , generate: [ "classes", "cols", "rows" ]
            }
          }
    menu =
      let
        -- | Still missing: anchorEl, onClose, MenuListProps, PopoverClasses, transitionDuration
        handler n = Tuple n eventHandler
        base = basePropsRow [] $ Map.fromFoldable $ Array.cons children $ map handler
          ["onEnter", "onEntered", "onEntering", "onExit", "onExited", "onExiting"]
      in simpleComponent
        { inherits: Nothing -- | We should inherit from Popover here
        , name: "Menu"
        , propsType:
          { base
          , generate: [ "autoFocus", "classes", "disableAutoFocusItem", "open", "transitionDuration", "variant" ]
          }
        }
    menuItem =
      let
        base = basePropsRow [] $ Map.fromFoldable
          [ children
          , component
          ]
      in simpleComponent
        { inherits: Nothing -- | TODO: inherit from `ListItem`
        , name: "MenuItem"
        , propsType:
          { base
          , generate: [ "classes", "dense", "disableGutters" ]
          }
        }
    touchRipple =
      { extraDeclarations: []
      , inherits: Just $ Type.constructor "React.Basic.DOM.Props_span"
      -- , name: touchRippleType.name
      , modulePath: touchRippleType.path
      , propsType:
        { base: emptyBase
        , generate: [ "center", "classes" ]
        }
      , tsc: { strictNullChecks: false }
      }
  in
    [ appBar, badge, buttonBase, button, fab, gridList, gridListTile, menu, menuItem, touchRipple ]

-- | XXX: Can we cleanup this last traverse?
multiString :: ∀ a. Pattern -> ReadM a -> ReadM (Array a)
multiString splitPattern read = do
  s <- readerAsk
  elems <-
    let
     strArray = filter (not <<< String.null) $ split splitPattern s
    in if Array.null strArray
      then readerError "Got empty string as input"
      else pure strArray
  let
    read' = unwrap read
  wrap $ for elems \elem -> lift $ runReaderT read' elem

componentOption :: Parser Component
componentOption =
  option componentRead (long "component" <> short 'c')

componentRead :: ReadM Component
componentRead = eitherReader $ \s -> case s `Map.lookup` components' of
  Just c -> pure c
  otherwise -> Left $ intercalate " "
    [ "Unkown component name"
    , show s
    ,". Please use one of:"
    , intercalate ", " (map show <<< List.sort <<< Map.Internal.keys $ components') <> "."
    ]
  where
    step c = Tuple (Model.componentName c) c
    components' = Map.fromFoldable $ map step components

iconOption :: Parser Icon
iconOption =
  option iconRead (long "icon" <> short 'i')

iconRead :: ReadM Icon
iconRead = eitherReader $ \s -> case s `Map.lookup` icons' of
  Just c -> pure c
  otherwise -> Left $ intercalate " "
    [ "Unkown icon name"
    , show s
    ,". Please use one of:"
    , intercalate ", " (List.sort <<< Map.Internal.keys $ icons') <> "."
    ]
  where
    step i = Tuple (iconName i) i
    icons' = Map.fromFoldable $ map step icons

commaSeparatedComponentList :: Parser (Array Component)
commaSeparatedComponentList = option (multiString (Pattern ",") componentRead)
    ( long "skip-props"
   <> short 's'
   <> metavar "component1,component2,...,component3"
   <> help helpText
   <> value [ ]
    )
  where
    helpText = intercalate "."
      [ "A comma-separated list of component names "
      , "which props should be dropped from props list."
      , "This can be useful if you want to list only component own "
      , "properties or destil some inheritance etc."
      ]

showPropsOptions :: Parser Options
showPropsOptions = map ShowPropsCommand $ { component: _, skip: _ }
  <$> componentOption
  <*> commaSeparatedComponentList -- (long "skip-components" <> short 's')

data GenOutput
  = Directory FilePath
  | Stdout

genOutput :: Parser GenOutput
genOutput = directory <|> stdout
  where
    directory = map Directory $ strOption $
      long "directory"
      <> short 'd'
      <> metavar "DIRECTORY"
      <> help "Write output components to directory"
    stdout = flag' Stdout $
      long "stdout"
      <> short 's'
      <> help "Print component to stdout"

genTarget :: Parser GenTarget
genTarget = genComponent <|> genComponents <|> genIcon
  where
    genComponent = map GenComponent componentOption
    genComponents = flag' GenComponents $
      long "all-components"
      <> short 'a'
      <> help "Codegen all components"
    genIcon = map GenIcon iconOption

genOptions :: Parser Options
genOptions = map Generate $ { target: _, output: _ }
  <$> genTarget
  <*> ((Just <$> genOutput) <|> pure Nothing)

data GenTarget
  = GenComponents
  | GenComponent Component
  -- | GenIcons
  | GenIcon Icon

data Options
  = ShowPropsCommand
    { component :: Component
    , skip :: Array Component
    }
  | Generate
    { target :: GenTarget
    , output :: Maybe GenOutput
    }

options :: Parser Options
options = subparser $
  command "codegen" (info genOptions (progDesc "Codegen all or a given module"))
  <> command "show-props" (info showPropsOptions (progDesc "Show information about props for a given component"))

main :: Effect Unit
main = do
  opts <- execParser (info (options <**> helper) fullDesc)
  let
    -- | Should I bring back multimodule handling logic?
    -- | For sure we want to keep track of the naming collisions
    -- | during codegen so maye we can just require that
    -- | we would have an unique naming strategy.
    writeComponentModules dir component code = launchAff_ $ do
      let
        js = Codegen (componentJSFile component) code.js
        ps = Codegen (componentPSFile component) code.ps
      Codegen.write dir js
      Codegen.write dir ps

    codegenComponent component output = runExceptT (Codegen.component component) >>= case _ of
        Right code -> case output of
          Just Stdout -> do
            log "\nPureScript:"
            log code.ps
            log "\nJavaScript:"
            log code.js
          Just (Directory d) -> do
            writeComponentModules d component code
          Nothing -> do
            writeComponentModules "../src" component code
        Left err -> log $ "Codegen errors: " <> intercalate "\n" err

    writeIconModules dir icon code = launchAff_ $ do
      let
        js = Codegen (iconJSFile icon) code.js
        ps = Codegen (iconPSFile icon) code.ps
      Codegen.write dir js
      Codegen.write dir ps

    iconCodegen icon = case _ of
      Just Stdout -> do
        log "\nPureScript:"
        log code.ps
        log "\nJavasScript:"
        log code.js
      Just (Directory d) -> do
        writeIconModules d icon code
      Nothing -> do
        writeIconModules "../src" icon code
      where
        code = Codegen.icon icon
  case opts of
    ShowPropsCommand { component, skip } -> do
      let
        getProps = do
          { props: c } <- TS.MUI.componentProps component
          s <- traverse (\s -> TS.MUI.componentProps s) skip <#> map _.props
          pure { component: c, skip: s }
      runExceptT getProps >>= case _ of
        Right { component: cProps, skip: s } -> do
          let
            cKeys = Map.keys cProps
            sKeys = Set.unions (map Map.keys s)
            keys = cKeys `Set.difference` sKeys
            props = Array.sort
              $ map (\(Tuple k v) -> k <> " : " <> cata pprintTypeName v.type)
              $ Map.toUnfoldable
              $ Map.filterWithKey (\k v -> k `Set.member` keys) cProps
          log $ intercalate "\n" props
        Left err -> log $ intercalate "\n" err
    Generate { target: GenComponent component, output } ->
      codegenComponent component output
    Generate { target: GenComponents, output } -> for_ components \component ->
      codegenComponent component output
    Generate { target: GenIcon i, output } ->
      iconCodegen i output

