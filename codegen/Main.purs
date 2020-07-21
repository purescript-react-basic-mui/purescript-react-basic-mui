module Codegen.Main where

import Prelude

import Codegen (Codegen(..), componentJSFile, componentPSFile, iconJSFile, iconPSFile, icons)
import Codegen (component, icon, write) as Codegen
import Codegen.AST (ModuleName(..), TypeName(..))
import Codegen.AST.Sugar.Type (constructor) as Type
import Codegen.Component (Component, Icon, ModulePath(..), Root(..), arrayJSX, iconName, jsx, psImportPath, rbProps)
import Codegen.Component (componentName) as Component
import Codegen.TS.MUI (componentProps) as TS.MUI
import Codegen.TS.Types (InstantiationStrategy(..))
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReader, runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (elem, null, sort) as Array
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (for_, intercalate)
import Data.Functor.Mu (Mu(..)) as Mu
import Data.Functor.Mu (unroll)
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
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Global.Unsafe (unsafeStringify)
import Matryoshka (cata)
import Node.Path (FilePath)
import Options.Applicative (Parser, ReadM, command, eitherReader, execParser, flag', fullDesc, help, helper, info, long, metavar, option, progDesc, readerError, short, strOption, subparser, value, (<**>))
import Options.Applicative.Types (readerAsk)
import ReadDTS.Instantiation (TypeF(..)) as Instantiation
import ReadDTS.Instantiation.Pretty (pprintTypeName)

components :: Array Component
components =
  let
    children = Tuple "children" arrayJSX

    eventHandlerProp name = Tuple name (Type.constructor "React.Basic.Events.EventHandler")

    foreignType = Type.constructor "Foreign.Foreign"

    jss = Type.constructor "MUI.Core.JSS"

    -- | It is nice to have parametrized `Row` by default
    -- | because it allows us
    -- basePropsRow props = props
    -- emptyBase = basePropsRow mempty

    simpleComponent { name, propsRow: { base, generate }, root } =
      { extraDeclarations: []
      , modulePath:
          { input: Name name
          , output: Name name
          }
      , propsRow:
        { base
        , generate
        , ts: { instantiation: Nothing, unionName: \_ _ → Nothing }
        }
      , root
      }

    touchRippleType =
      { path: (Path "ButtonBase" (Name "TouchRipple"))
      , name: "TouchRipple"
      }

    appBar =
      simpleComponent
        { name: "AppBar"
        , propsRow:
          { base:  Map.fromFoldable [ children ]
          , generate: [ "classes", "color", "position" ]
          }
        , root: MUIComponent paper
        }

    -- avatar =
    --   simpleComponent
    --     { inherits: Nothing
    --     , name: "Avatar"
    --     , propsRow:
    --       { base:  Map.fromFoldable []
    --       , generate:
    --         [ "alt"
    --         , "classes"
    --         -- not sure what to do here, "imgProps"
    --         , "sizes"
    --         , "src"
    --         , "srcSet"
    --         , "variant"
    --         ]
    --       }
    --     }

    -- backdrop =
    --   simpleComponent
    --     { inherits: Just $ MUI.rList
    --         [ Type.constructor "MUI.Core.Fade.FadePropsRow"
    --         , divProps
    --         ]
    --     , name: "Backdrop"
    --     , propsRow:
    --       { base: Map.fromFoldable
    --           [ children
    --           , Tuple "style" (Type.constructor "React.Basic.DOM.CSS")
    --           ]
    --       , generate: [ "classes", "invisible", "open", "transitionDuration" ]
    --       }
    --     }

    badge =
      simpleComponent
        { root: rbProps.div
        , name: "Badge"
        , propsRow:
          { base: Map.fromFoldable
              [ Tuple "badgeContent" jsx
              , children
              ]
          , generate:
            [ "anchorOrigin"
            , "classes"
            , "color"
            , "invisible"
            , "max"
            , "showZero"
            , "variant"
            ]
          }
        }

    ---- | `children` and `component` are taken from `buttonBase`
    --bottomNavigation =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "BottomNavigation"
    --    , propsRow:
    --      { base:  Map.fromFoldable
    --          [ children
    --          -- , component
    --          , eventHandlerProp "onChange"
    --          ]
    --      , generate: [ "classes", "showLabels" ]
    --      }
    --    }

    --box =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "Box"
    --    , propsRow:
    --      { base:  Map.fromFoldable $ [ children, Tuple "css" jss ]
    --      , generate: [ "clone" ]
    --      }
    --    }

    --breadcrumbs =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "Breadcrumbs"
    --    , propsRow:
    --      { base:
    --         Map.fromFoldable
    --          $ [ children
    --            -- Not found on the TS side
    --            -- , component
    --            , Tuple "separator" jsx
    --            , Tuple "ref" foreignType
    --            ]
    --      , generate: [ "classes", "itemsAfterCollapse", "itemsBeforeCollapse", "maxItems" ]
    --      }
    --    }
    buttonBase =
      -- let
      --   buttonBaseActions = declType (TypeName "ButtonBaseActions") [] foreignType
      --   buttonBaseTypeProps = declType (TypeName "ButtonBaseTypeProp") [] foreignType
      -- in
        { extraDeclarations: []
          -- [ buttonBaseActions.declaration
          -- , buttonBaseTypeProps.declaration
          -- ]
        , modulePath:
          { input: Name "ButtonBase"
          , output: Name "ButtonBase"
          }
        , propsRow:
          { base:  Map.fromFoldable
              [ Tuple "action" foreignType
              , Tuple "buttonRef" foreignType
              , eventHandlerProp "onFocusVisible"
              -- | I'm not sure hot to handle this kind of props parameter
              -- | in the current architecture.
              -- , Tuple "TouchRippleProps" $ Type.recordLiteral $ Type.app
              --     ( roll $ TypeConstructor
              --         { moduleName: Just $ ModuleName (psImportPath (componentFullPath touchRipple))
              --         , name: TypeName $ (propsRowTypeName touchRippleType.name)
              --         }
              --     )
              --     (Array.fromFoldable touchRipple.inherits)
              ]
          , generate:
            [ "centerRipple"
            , "classes"
            , "color"
            , "disabled"
            , "disableRipple"
            , "focusRipple"
            , "focusVisibleClassName"
            , "type"
            ]
          , ts: { instantiation: Nothing, unionName: \_ _ → Nothing }
          }
        , root: rbProps.button
        }

    button =
      simpleComponent
        { name: "Button"
        , propsRow:
          { base: Map.fromFoldable
              [ Tuple "endIcon" jsx
              , Tuple "startIcon" jsx
              ]
          , generate:
            [ "classes"
            , "color"
            , "disabled"
            , "disableFocusRipple"
            , "disableRipple"
            , "fullWidth"
            , "href"
            , "size"
            , "variant"
            ]
          }
        , root: MUIComponent buttonBase
        }

    buttonGroup = simpleComponent
      { name: "ButtonGroup"
      , propsRow:
        { base:
           Map.fromFoldable [ children ]
        , generate:
          [ "classes"
          , "color"
          , "disabled"
          , "disableFocusRipple"
          , "disableRipple"
          , "fullWidth"
          , "orientation"
          , "size"
          , "variant"
          ]
        }
      , root: rbProps.div
      }

    ---- | TODO: make value a type variable
    --bottomNavigationAction =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsRow"
    --        , divProps
    --        ]
    --    , name: "BottomNavigationAction"
    --    , propsRow:
    --      { base: Map.fromFoldable
    --          [ Tuple "icon" jsx
    --          , Tuple "label" jsx
    --          ]
    --      , generate:
    --        [ "classes"
    --        , "showLabel"
    --        , "selected"
    --        ]
    --      }
    --    }

    --card =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Paper.PaperPropsRow"
    --        , divProps
    --        ]
    --    , name: "Card"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ children ]
    --      , generate: [ "classes", "raised" ]
    --      }
    --    }

    --cardActionArea =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList'
    --        [ "MUI.Core.ButtonBase.ButtonBasePropsRow"
    --        , "MUI.DOM.Generated.Props_button"
    --        ]
    --    , name: "CardActionArea"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ children ]
    --      , generate: [ "classes" ]
    --      }
    --    }

    --cardActions =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "CardActions"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ children ]
    --      , generate: [ "classes", "disableSpacing" ]
    --      }
    --    }

    --cardContent =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "CardContent"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ children ]
    --      , generate: [ "classes" ]
    --      }
    --    }

    --cardHeader =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "CardHeader"
    --    , propsRow:
    --      { base:  Map.fromFoldable
    --          [ Tuple "action" jsx
    --          , Tuple "avatar" jsx
    --          , children
    --          , Tuple "subheader" jsx
    --          , Tuple "subheaderTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyOpaqueProps")
    --          , Tuple "title" jsx
    --          , Tuple "titleTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyOpaqueProps")
    --          ]
    --      , generate: [ "classes", "disableTypography" ]
    --      }
    --    }

    --cardMedia =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "CardMedia"
    --    , propsRow:
    --      { base:  Map.fromFoldable
    --          [ children
    --          -- This isn't being found in the .d.ts
    --          --, component
    --          ]
    --      , generate: [ "classes", "image", "src" ]
    --      }
    --    }

    --checkbox =
    --  simpleComponent
    --    { inherits: Nothing -- should be IconButon
    --    , name: "Checkbox"
    --    , propsRow:
    --      { base: Map.fromFoldable
    --          ( [ Tuple "checkedIcon" jsx
    --            , Tuple "icon" jsx
    --            , Tuple "indeterminateIcon" jsx
    --            , Tuple "inputProps" foreignType
    --            , Tuple "inputRef" foreignType
    --            , Tuple "value" foreignType
    --            ]
    --              <> (map eventHandlerProp [ "onChange" ])
    --          )
    --      , generate:
    --        [ "checked"
    --        , "classes"
    --        , "color"
    --        , "disabled"
    --        , "disableRipple"
    --        , "id"
    --        , "indeterminate"
    --        , "required"
    --        ]
    --      }
    --    }

    --chip =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "Chip"
    --    , propsRow:
    --      { base:  Map.fromFoldable
    --        [ Tuple "avatar" jsx
    --        , Tuple "deleteIcon" jsx
    --        , Tuple "icon" jsx
    --        , Tuple "label" jsx
    --        , eventHandlerProp "onDelete"
    --        ]
    --      , generate:
    --        [ "classes"
    --        , "color"
    --        , "disabled"
    --        , "size"
    --        , "variant"
    --        ]
    --      }
    --    }

    --circularProgress =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "CircularProgress"
    --    , propsRow:
    --      { base:  Map.fromFoldable []
    --      , generate:
    --        [ "classes"
    --        , "color"
    --        , "disableShrink"
    --        , "size"
    --        , "thickness"
    --        , "value"
    --        , "variant"
    --        ]
    --      }
    --    }

    --clickAwayListener =
    --  let
    --    onClickAway = eventHandlerProp "onClickAway"

    --    -- | Single jsx node is required
    --    child = Tuple "children" jsx

    --    base =  Map.fromFoldable [ child, onClickAway ]
    --  in
    --    simpleComponent
    --      { inherits: Nothing
    --      , name: "ClickAwayListener"
    --      , propsRow:
    --        { base: emptyBase
    --        , generate: [ "mouseEvent", "touchEvent" ]
    --        }
    --      }

    --collapse =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "Collapse"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ children ]
    --      , generate:
    --        [ "collapsedHeight"
    --        , "in"
    --        , "timeout"
    --        ]
    --      }
    --    }

    container = simpleComponent
      { name: "Container"
      , propsRow:
        { base: mempty
        , generate:
          [ "disableGutters"
          , "fixed"
          , "maxWidth"
          ]
        }
      , root: rbProps.div
      }

    --cssBaseline =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "CssBaseline"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ children ]
    --      , generate: []
    --      }
    --    }

    --dialog =
    --  let
    --    -- | TODO:
    --    -- | migration.
    --    -- | * `PaperComponent`, `PaperProps`, `TransitionComponent`, `TransitionDuration`
    --    handlers =
    --      map eventHandlerProp
    --        [ "onEnter"
    --        , "onEntered"
    --        , "onEntering"
    --        , "onExit"
    --        , "onExited"
    --        , "onExiting"
    --        ]

    --    base =  Map.fromFoldable $ [ children ] <> handlers
    --  in
    --    simpleComponent
    --      { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Modal.ModalPropsRow"
    --        , divProps
    --        ]
    --      , name: "Dialog"
    --      , propsRow:
    --        { base: emptyBase
    --        , generate:
    --          [ "aria-describedby"
    --          , "aria-labelledby"
    --          , "classes"
    --          , "fullScreen"
    --          , "fullWidth"
    --          , "maxWidth"
    --          , "scroll"
    --          , "transitionDuration"
    --          ]
    --        }
    --      }

    --dialogActions =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "DialogActions"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ children ]
    --      , generate: [ "classes", "disableSpacing" ]
    --      }
    --    }

    --dialogContent =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "DialogContent"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ children ]
    --      , generate: [ "classes", "dividers" ]
    --      }
    --    }

    --dialogTitle =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "DialogTitle"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ children ]
    --      , generate: [ "classes", "disableTypography" ]
    --      }
    --    }

    ---- | TODO: add component
    divider =
      simpleComponent
        { name: "Divider"
        , propsRow:
          { base:  Map.fromFoldable []
          , generate:
            [ "absolute"
            , "classes"
            , "light"
            , "orientation"
            , "variant"
            ]
          }
        , root: rbProps.hr
        }

    --drawer =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Modal.ModalPropsRow"
    --        , divProps
    --        ]
    --    , name: "Drawer"
    --    , propsRow:
    --      { base: Map.fromFoldable
    --          ( [ children
    --            , Tuple "ModalProps" (Type.constructor "MUI.Core.Modal.ModalOpaqueProps")
    --            , eventHandlerProp "onClose"
    --            , Tuple "PaperProps" (Type.constructor "MUI.Core.Modal.ModalOpaqueProps")
    --            , Tuple "SlideProps" (Type.constructor "MUI.Core.Slide.SlideOpaqueProps")
    --            ]
    --              <> map eventHandlerProp [ "onClose", "onEnter", "onEntered", "onEntering", "onExit", "onExited", "onExiting" ]
    --          )
    --      , generate:
    --        [ "anchor"
    --        , "classes"
    --        , "elevation"
    --        , "open"
    --        , "transitionDuration"
    --        , "variant"
    --        ]
    --      }
    --    }

    ---- | TODO: TransitionComponent, TransitionProps
    --expansionPanel =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Paper.PaperPropsRow"
    --        , divProps
    --        ]
    --    , name: "ExpansionPanel"
    --    , propsRow:
    --      { base: Map.fromFoldable
    --          [ children
    --          , eventHandlerProp "onChange"
    --          ]
    --      , generate:
    --        [ "classes"
    --        , "defaultExpanded"
    --        , "disabled"
    --        , "expanded"
    --        ]
    --      }
    --    }

    --expansionPanelActions =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "ExpansionPanelActions"
    --    , propsRow:
    --      { base: Map.fromFoldable [ children ]
    --      , generate: [ "classes" ]
    --      }
    --    }

    --expansionPanelDetails =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "ExpansionPanelDetails"
    --    , propsRow:
    --      { base: Map.fromFoldable [ children ]
    --      , generate: [ "classes" ]
    --      }
    --    }

    --expansionPanelSummary =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "ExpansionPanelSummary"
    --    , propsRow:
    --      { base: Map.fromFoldable
    --          [ children
    --          , Tuple "expandIcon" jsx
    --          , Tuple "IconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonOpaqueProps")
    --          ]
    --      , generate: [ "classes" ]
    --      }
    --    }

    --fab =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList'
    --        [ "MUI.Core.ButtonBase.ButtonBasePropsRow"
    --        , "MUI.DOM.Generated.Props_button"
    --        ]
    --    , name: "Fab"
    --    , propsRow:
    --      { base: emptyBase
    --      , generate:
    --        [ "classes"
    --        , "color"
    --        , "disabled"
    --        , "disableFocusRipple"
    --        , "href"
    --        , "size"
    --        , "variant"
    --        ]
    --      }
    --    }

    ---- | TODO: TransitionComponent
    --fade =
    --  simpleComponent
    --    { inherits: Nothing -- should inherit TransitionComponent
    --    , name: "Fade"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ Tuple "ref" foreignType ]
    --      , generate:
    --        [ {-- not sure what to do here, "theme" --}]
    --      }
    --    }

    ---- | TODO: inputComponent, make value a type variable
    --filledInput =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.Core.InputBasePropsRow" ]
    --    , name: "FilledInput"
    --    , propsRow:
    --      { base:  Map.fromFoldable
    --          [ children
    --          , Tuple "endAdornment" jsx
    --          , Tuple
    --              "inputProps"
    --              (Type.constructor "MUI.Core.InputBaseOpaqueProps")
    --          , Tuple "inputRef" foreignType
    --          , eventHandlerProp "onChange"
    --          , Tuple "startAdornment" jsx
    --          , Tuple "value" foreignType
    --          ]
    --      , generate:
    --          [ "autoComplete"
    --          , "autoFocus"
    --          , "classes"
    --          , "className"
    --          , "color"
    --          , "defaultValue"
    --          , "disabled"
    --          , "disableUnderline"
    --          , "error"
    --          , "fullWidth"
    --          , "id"
    --          , "margin"
    --          , "multiline"
    --          , "name"
    --          , "placeholder"
    --          , "readOnly"
    --          , "required"
    --          , "rows"
    --          , "rowsMax"
    --          , "type"
    --          ]
    --      }
    --    }

    formControl =
      simpleComponent
        { name: "FormControl"
        , propsRow:
          { base: Map.fromFoldable [ children ]
          , generate:
            [ "classes"
            , "color"
            , "disabled"
            , "error"
            , "fullWidth"
            , "hiddenLabel"
            , "margin"
            , "required"
            , "variant"
            ]
          }
        , root: rbProps.div
        }

    formControlLabel =
      simpleComponent
        { name: "FormControlLabel"
        , propsRow:
          { base: Map.fromFoldable
              [ Tuple "control" jsx
              , Tuple "label" jsx
              , eventHandlerProp "onChange"
              , Tuple "value" foreignType
              ]
          , generate:
            [ "checked"
            , "classes"
            , "disabled"
            , "labelPlacement"
            , "name"
            ]
          }
        , root: rbProps.label
        }

    formGroup =
      simpleComponent
        { name: "FormGroup"
        , propsRow:
          { base: Map.fromFoldable [ children ]
          , generate:
            [ "classes"
            , "row"
            ]
          }
        , root: rbProps.div
        }

    formHelperText =
      simpleComponent
        { name: "FormHelperText"
        , propsRow:
          { base: Map.fromFoldable [ children ]
          , generate:
            [ "classes"
            , "disabled"
            , "error"
            , "filled"
            , "focused"
            , "margin"
            , "required"
            , "variant"
            ]
          }
        , root: rbProps.div
        }

    formLabel =
      simpleComponent
        { name: "FormLabel"
        , propsRow:
          { base: Map.fromFoldable [ children ]
          , generate:
            [ "classes"
            , "color"
            , "disabled"
            , "error"
            , "filled"
            , "focused"
            , "required"
            ]
          }
        , root: rbProps.label
        }

    grid =
        { extraDeclarations: []
        , modulePath:
            { input: Name "Grid"
            , output: Name "Grid"
            }
        , propsRow:
          { base: Map.fromFoldable [ children ]
          , generate:
            [ "alignContent"
            , "alignItems"
            , "classes"
            , "container"
            , "direction"
            , "item"
            , "justify"
            , "lg"
            , "md"
            , "sm"
            , "spacing"
            , "wrap"
            , "xl"
            , "xs"
            , "zeroMinWidth"
            ]
          , ts:
              { instantiation: Nothing
              , unionName: \property members → case property of
                  _ | property `Array.elem` ["xs", "sm", "md" , "lg", "xl"] →
                    Just $ TypeName "GridSize"
                  "spacing" →
                    Just $ TypeName "GridSpacing"
                  "justify" →
                    Just $ TypeName "GridJustification"
                  otherwise → Nothing
              }
          }
        , root: rbProps.div
        }

    --gridList = simpleComponent
    --  { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_ul" ]
    --  , name: "GridList"
    --  , propsRow:
    --    { base: Map.fromFoldable [ children ]
    --    , generate: [ "cellHeight", "classes", "cols", "spacing" ]
    --    }
    --  }

    --gridListTile = simpleComponent
    --  { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_li" ]
    --  , name: "GridListTile"
    --  , propsRow:
    --    { base: Map.fromFoldable [ children ]
    --    , generate: [ "classes", "cols", "rows" ]
    --    }
    --  }

    --gridListTileBar =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "GridListTileBar"
    --    , propsRow:
    --      { base: Map.fromFoldable
    --          [ Tuple "actionIcon" jsx
    --          , Tuple "subtitle" jsx
    --          , Tuple "title" jsx
    --          ]
    --      , generate:
    --        [ "classes"
    --        , "actionPosition"
    --        , "titlePosition"
    --        ]
    --      }
    --    }

    ---- | TODO: update when Transition is figured out
    --grow =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "Grow"
    --    , propsRow:
    --      { base: emptyBase
    --      , generate: [ "in" , "timeout" ]
    --      }
    --    }

    -- | It seems that on the current master
    -- | `HiddenCss` uses `div` as a root
    -- | but `HiddenJs` returns just children:
    -- |
    -- | * https://github.com/mui-org/material-ui/blob/60d99a39836fb82f4da1477a717f642c216fb0b9/packages/material-ui/src/Hidden/HiddenCss.js#L77
    -- |
    -- | * https://github.com/mui-org/material-ui/blob/60d99a39836fb82f4da1477a717f642c216fb0b9/packages/material-ui/src/Hidden/HiddenJs.js#L51
    -- |
    -- | In other words root will be ignored when they
    -- | are provided to the component.
    hidden =
        { extraDeclarations: []
        , modulePath:
          { input: Name "Hidden"
          , output: Name "Hidden"
          }
        , propsRow:
          { base: mempty -- Map.fromFoldable [ Tuple "only" foreignType ]
          , generate:
            [ "implementation"
            , "initialWidth"
            , "lgDown"
            , "lgUp"
            , "mdDown"
            , "mdUp"
            , "only"
            , "smDown"
            , "smUp"
            , "xlDown"
            , "xlUp"
            , "xsDown"
            , "xsUp"
            ]
          , ts:
            { instantiation: Nothing
            -- | We got somewhat unsafe but convenient
            -- | API I think:
            -- |
            -- | only::{
            -- |   lg :: Only,
            -- |   md :: Only,
            -- |   only :: Array  Only  ->  Only,
            -- |   sm :: Only,
            -- |   xl :: Only,
            -- |   xs :: Only
            -- |  }
            , unionName: case _ of
                "Anonymous" → const $ Just $ TypeName "Only"
                otherwise → const $ Nothing
            }
          }
        , root: rbProps.div
        }

    --icon =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_span" ]
    --    , name: "Icon"
    --    , propsRow:
    --      { base: emptyBase
    --      , generate:
    --        [ "classes"
    --        , "color"
    --        , "fontSize"
    --        ]
    --      }
    --    }

    iconButton = simpleComponent
      { name: "IconButton"
      , propsRow:
        { base: Map.fromFoldable [ children ]
        , generate:
          [ "classes"
          , "color"
          , "disabled"
          , "disableFocusRipple"
          , "disableRipple"
          , "edge"
          , "size"
          ]
        }
      , root: MUIComponent buttonBase
      }

    input =
      simpleComponent
        { name: "Input"
        , propsRow:
          { base: Map.fromFoldable
            [ Tuple "defaultValue" foreignType
            , Tuple "endAdornment" jsx
            , Tuple "inputProps" foreignType
            , Tuple "inputRef" foreignType
            , Tuple "startAdornment" jsx
            , Tuple "value" foreignType
            , eventHandlerProp "onChange"
            ]
          , generate:
            [ "autoComplete"
            , "autoFocus"
            , "classes"
            , "className"
            , "color"
            , "disabled"
            , "error"
            , "fullWidth"
            , "id"
            , "margin"
            , "multiline"
            , "name"
            , "placeholder"
            , "readOnly"
            , "required"
            , "rows"
            , "rowsMax"
            , "type"
            ]
          }
        , root: MUIComponent inputBase
        }

    --inputAdornment =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "InputAdornment"
    --    , propsRow:
    --      { base:
    --           Map.fromFoldable [ children ]
    --      , generate:
    --        [ "classes"
    --        , "disablePointerEvents"
    --        , "disableTypography"
    --        , "position"
    --        , "variant"
    --        ]
    --      }
    --    }

    ---- | TODO: inputProps should be something like ReactComponent { | InputProps }
    inputBase = simpleComponent
      { name: "InputBase"
      , propsRow:
        { base: Map.fromFoldable
            [ Tuple "defaultValue" foreignType
            , Tuple "endAdornment" jsx
            , Tuple "inputProps" foreignType
            , Tuple "inputRef" foreignType
            , Tuple "startAdornment" jsx
            , Tuple "value" foreignType
            , eventHandlerProp "onChange"
            ]
        , generate:
          [ "autoComplete"
          , "autoFocus"
          , "classes"
          , "className"
          , "color"
          , "disabled"
          , "error"
          , "fullWidth"
          , "id"
          , "margin"
          , "multiline"
          , "name"
          , "placeholder"
          , "readOnly"
          , "required"
          , "rows"
          , "rowsMax"
          , "type"
          ]
        }
      , root: rbProps.div
      }

    inputLabel =
      simpleComponent
        { name: "InputLabel"
        , propsRow:
          { base:
              Map.fromFoldable
                  [ children
                  ]
          , generate:
            [ "classes"
            , "color"
            , "disableAnimation"
            , "disabled"
            , "error"
            , "focused"
            , "margin"
            , "required"
            , "shrink"
            , "variant"
            ]
          }
        , root: MUIComponent formLabel
          -- [ Type.constructor "MUI.Core.FormLabel.FormLabelPropsRow", Type.constructor "MUI.DOM.Generated.Props_label" ]
        }

    --linearProgress =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "LinearProgress"
    --    , propsRow:
    --      { base: emptyBase
    --      , generate:
    --        [ "classes"
    --        , "color"
    --        , "value"
    --        , "valueBuffer"
    --        , "variant"
    --        ]
    --      }
    --    }

    link =
      simpleComponent
        { name: "Link"
        , propsRow:
          { base: Map.fromFoldable
              [ children
              -- , Tuple "TypographyClasses" (Type.constructor "MUI.Core.Typography.TypographyClassesKey")
              ]
          , generate:
            [ "classes"
            , "color"
            , "underline"
            , "variant"
            ]
          }
        , root: rbProps.a
        }

    --list =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_ul" ]
    --    , name: "List"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "subheader" jsx
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "dense"
    --        , "disablePadding"
    --        ]
    --      }
    --    }

    ---- | TODO: add ContainerComponent and ContainerProps
    --listItem =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_li" ]
    --    , name: "ListItem"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "alignItems"
    --        , "autoFocus"
    --        , "button"
    --        , "classes"
    --        , "dense"
    --        , "disabled"
    --        , "disableGutters"
    --        , "divider"
    --        , "selected"
    --        ]
    --      }
    --    }

    --listItemAvatar =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "ListItemAvatar"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "classes"
    --        ]
    --      }
    --    }

    --listItemIcon =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "ListItemIcon"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ -- children
    --              ]
    --      , generate:
    --        [ "classes"
    --        ]
    --      }
    --    }

    --listItemSecondaryAction =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "ListItemSecondaryAction"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "classes"
    --        ]
    --      }
    --    }

    --listItemText =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "ListItemText"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ Tuple "primary" jsx
    --              , Tuple "primaryTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyClassesKey")
    --              , Tuple "secondary" jsx
    --              , Tuple "secondaryTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyClassesKey")
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "disableTypography"
    --        , "inset"
    --        ]
    --      }
    --    }

    --listSubheader =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_li" ]
    --    , name: "ListSubheader"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "color"
    --        , "disableGutters"
    --        , "disableSticky"
    --        , "inset"
    --        ]
    --      }
    --    }

    --menu =
    --  let
    --    -- | Still missing: anchorEl, onClose, MenuListProps, PopoverClasses, transitionDuration
    --    handlers =
    --      map eventHandlerProp
    --        [ "onClose", "onEnter", "onEntered", "onEntering", "onExit", "onExited", "onExiting" ]

    --    -- | I'm not sure what is the difference between `React.Element` and `DOM.Element`
    --    nullable = Type.constructor "Data.Nullable.Nullable"

    --    domElement = Type.constructor "Web.DOM.Element"

    --    anchorEl = Tuple "anchorEl" $ Type.app nullable [ domElement ]

    --    base =  Map.fromFoldable $ [ anchorEl, children ] <> handlers
    --  in
    --    simpleComponent
    --      { inherits: Nothing -- | We should inherit from Popover here
    --      , name: "Menu"
    --      , propsRow:
    --        { base: emptyBase
    --        , generate: [ "autoFocus", "classes", "disableAutoFocusItem", "open", "transitionDuration", "variant" ]
    --        }
    --      }

    --menuItem =
    --  let
    --    base =
    --        Map.fromFoldable
    --            [ children
    --            -- XXX: We are catching material ui documentation / type error here.
    --            -- MenuItem doesn't contain `component` prop.
    --            -- , component
    --            ]
    --  in
    --    simpleComponent
    --      { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.ListItem.ListItemPropsRow"
    --        , Type.constructor "MUI.DOM.Generated.Props_li"
    --        ]
    --      , name: "MenuItem"
    --      , propsRow:
    --        { base: emptyBase
    --        , generate: [ "classes", "dense", "disableGutters" ]
    --        }
    --      }

    --mobileStepper =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Paper.PaperPropsRow", Type.constructor "MUI.DOM.Generated.Props_props" ]
    --    , name: "MobileStepper"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ Tuple "backButton" jsx
    --              , Tuple "LinearProgressProps" (Type.constructor "MUI.Core.LinearPropgress.LinearProgressOpaqueProps")
    --              , Tuple "nextButton" jsx
    --              ]
    --      , generate:
    --        [ "activeStep"
    --        , "backButton"
    --        , "classes"
    --        , "position"
    --        , "steps"
    --        , "variant"
    --        ]
    --      }
    --    }

    --modal =
    --  let
    --    handlers =
    --      map eventHandlerProp
    --        [ "onBackdropClick"
    --        , "onClose"
    --        , "onEscapeKeyDown"
    --        , "onRendered"
    --        ]

    --    backdropOpaqueProps = Type.constructor "MUI.Core.Backdrop.BackdropOpaqueProps"

    --    base =
    --       Map.fromFoldable
    --        $ [ children
    --          , Tuple "BackdropComponent" (reactComponentApply backdropOpaqueProps)
    --          , Tuple "BackdropProps" backdropOpaqueProps
    --          -- , container
    --          , Tuple
    --              "manager"
    --              (Type.constructor "MUI.Core.Modal.ModalManager.ModalManager")
    --          ]
    --        <> handlers
    --  in
    --    simpleComponent
    --      { inherits: Nothing
    --      , name: "Modal"
    --      , propsRow:
    --        { base: emptyBase
    --        , generate:
    --          [ "closeAfterTransition"
    --          , "disableAutoFocus"
    --          , "disableBackdropClick"
    --          , "disableEnforceFocus"
    --          , "disableEscapeKeyDown"
    --          , "disablePortal"
    --          , "disableRestoreFocus"
    --          , "disableScrollLock"
    --          , "hideBackdrop"
    --          , "keepMounted"
    --          , "open"
    --          ]
    --        }
    --      }

    ---- | TODO: value
    --nativeSelect =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Input.InputPropsRow", divProps ]
    --    , name: "NativeSelect"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "IconComponent" jsx
    --              , Tuple "input" jsx
    --              , Tuple "inputProps" (Type.constructor "MUI.Core.Input.InputOpaqueProps")
    --              , eventHandlerProp "onChange"
    --              , Tuple "value" foreignType
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "variant"
    --        ]
    --      }
    --    }

    ---- | TODO: value
    --noSsr =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "NoSsr"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "fallback" jsx
    --              ]
    --      , generate:
    --        [ "defer"
    --        ]
    --      }
    --    }

    ---- | inputComponent
    --outlineInput =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.InputBase.InputBasePropsRow", divProps ]
    --    , name: "OutlinedInput"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ Tuple "defaultValue" foreignType
    --              , Tuple "endAdornment" jsx
    --              , Tuple "inputProps" foreignType
    --              , Tuple "inputRef" foreignType
    --              , Tuple "startAdornment" jsx
    --              , Tuple "value" foreignType
    --              , eventHandlerProp "onChange"
    --              ]
    --      , generate:
    --        [ "autoComplete"
    --        , "autoFocus"
    --        , "classes"
    --        , "className"
    --        , "color"
    --        , "disabled"
    --        , "error"
    --        , "fullWidth"
    --        , "id"
    --        , "margin"
    --        , "multiline"
    --        , "name"
    --        , "notched"
    --        , "placeholder"
    --        , "readOnly"
    --        , "required"
    --        , "rows"
    --        , "rowsMax"
    --        , "type"
    --        ]
    --      }
    --    }

    paper =
      simpleComponent
        { name: "Paper"
        , propsRow:
          { base:  Map.fromFoldable $ [ children ]
          , generate:
            [ "classes"
            , "elevation"
            , "square"
            ]
          }
        , root: rbProps.div
        }

    ---- | TransitionComponent, TransitionProps, transformOrigin
    --popover =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Modal.ModalPropsRow", divProps ]
    --    , name: "Popover"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ Tuple "action" foreignType
    --              , Tuple "anchorEl" foreignType
    --              , Tuple "anchorPosition" foreignType
    --              , children
    --              , Tuple "getContentAnchorEl" foreignType
    --              , eventHandlerProp "onChange"
    --              , eventHandlerProp "onEnter"
    --              , eventHandlerProp "onEntering"
    --              , eventHandlerProp "onExit"
    --              , eventHandlerProp "onExited"
    --              , eventHandlerProp "onExiting"
    --              , Tuple "PaperProps" (Type.constructor "MUI.Core.Paper.PaperOpaqueProps")
    --              ]
    --      , generate:
    --        [ "anchorOrigin"
    --        , "anchorPosition"
    --        , "classes"
    --        , "elevation"
    --        , "marginThreshold"
    --        , "open"
    --        --, "transformOrigin"
    --        , "transitionDuration"
    --        ]
    --      }
    --    }

    --popper =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "Popper"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ Tuple "anchorEl" foreignType
    --              , children
    --              , Tuple "modifiers" foreignType
    --              , Tuple "popperOptions" foreignType
    --              , Tuple "popperRef" foreignType
    --              ]
    --      , generate:
    --        [ "disablePortal"
    --        , "keepMounted"
    --        , "open"
    --        , "transition"
    --        ]
    --      }
    --    }

    --portal =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "Portal"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "container" foreignType
    --              , eventHandlerProp "onRendered"
    --              ]
    --      , generate: [ "disablePortal" ]
    --      }
    --    }

    ---- | value, inputProps, inputRef
    --radio =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.IconButton.IconButtonPropsRow", Type.constructor "MUI.DOM.Generated.Props_button" ]
    --    , name: "Radio"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ Tuple "checkedIcon" jsx
    --              , Tuple "icon" jsx
    --              , Tuple "inputProps" foreignType
    --              , Tuple "inputRef" foreignType
    --              , eventHandlerProp "onChange"
    --              , Tuple "value" foreignType
    --              ]
    --      , generate:
    --        [ "checked"
    --        , "classes"
    --        , "color"
    --        , "disabled"
    --        , "disableRipple"
    --        , "id"
    --        , "name"
    --        , "required"
    --        ]
    --      }
    --    }

    ---- | value, inputProps, inputRef
    --radioGroup =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.FormGroup.FormGroupPropsRow", divProps ]
    --    , name: "RadioGroup"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "defaultValue" foreignType
    --              , eventHandlerProp "onChange"
    --              , Tuple "value" foreignType
    --              ]
    --      , generate:
    --        [ "name"
    --        ]
    --      }
    --    }

    --rootRef =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "RootRef"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ Tuple "rootRef" foreignType
    --              ]
    --      , generate:
    --        []
    --      }
    --    }

    ---- | TODO: value
    --select =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Input.InputPropsRow", divProps ]
    --    , name: "Select"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "IconComponent" jsx
    --              , Tuple "input" jsx
    --              , Tuple "inputProps" (Type.constructor "MUI.Core.Input.InputOpaqueProps")
    --              , Tuple "MenuProps" (Type.constructor "MUI.Core.Menu.MenuOpaqueProps")
    --              , eventHandlerProp "onChange"
    --              , eventHandlerProp "onClose"
    --              , eventHandlerProp "onOpen"
    --              , Tuple "renderValue" foreignType
    --              , Tuple "SelectDisplayProps" foreignType
    --              , Tuple "value" foreignType
    --              ]
    --      , generate:
    --        [ "autoWidth"
    --        , "classes"
    --        , "displayEmpty"
    --        , "labelWidth"
    --        , "multiple"
    --        , "native"
    --        , "open"
    --        , "renderValue"
    --        , "variant"
    --        ]
    --      }
    --    }

    --slide =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "Slide"
    --    , propsRow:
    --      { base:
    --         Map.fromFoldable
    --          $ map eventHandlerProp [ "onEnter", "onEntered", "onEntering", "onExit", "onExited", "onExiting" ]
    --      , generate:
    --        [ "direction", "in", "timeout"
    --        ]
    --      }
    --    }

    ---- | TODO: ThumbComponent ValueLabelComponent
    --slider =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_span" ]
    --    , name: "Slider"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "defaultValue" foreignType
    --              , Tuple "getAriaLabel" foreignType
    --              , Tuple "getAriaValueText" foreignType
    --              , eventHandlerProp "onChange"
    --              , eventHandlerProp "onChangeCommitted"
    --              , Tuple "marks" foreignType
    --              , Tuple "value" foreignType
    --              , Tuple "valueLabelFormat" foreignType
    --              ]
    --      , generate:
    --        [ "aria-label"
    --        , "aria-labelledby"
    --        , "aria-valuetext"
    --        , "classes"
    --        , "color"
    --        , "disabled"
    --        , "max"
    --        , "min"
    --        , "name"
    --        , "orientation"
    --        , "step"
    --        , "track"
    --        , "valueLabelDisplay"
    --        ]
    --      }
    --    }

    ---- | TODO: TransitionComponent, TransitionProps
    --snackbar =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "Snackbar"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "action" jsx
    --              , Tuple "ClickAwayListenerProps" (Type.constructor "MUI.Core.ClickAwayListener.ClickAwayListenerOpaqueProps")
    --              , Tuple "ContentProps" (Type.constructor "MUI.Core.SnackbarContent.SnackbarContentOpaqueProps")

    --              -- `key` is in the docs but not in the typedef
    --              --, Tuple "key" foreignType

    --              , Tuple "message" jsx
    --              , eventHandlerProp "onClose"
    --              , eventHandlerProp "onEnter"
    --              , eventHandlerProp "onEntered"
    --              , eventHandlerProp "onEntering"
    --              , eventHandlerProp "onExit"
    --              , eventHandlerProp "onExited"
    --              , eventHandlerProp "onExiting"
    --              ]
    --      , generate:
    --        [ "anchorOrigin"
    --        , "autoHideDuration"
    --        , "classes"
    --        , "disableWindowBlurListener"
    --        , "open"
    --        , "resumeHideDuration"
    --        , "transitionDuration"
    --        ]
    --      }
    --    }

    --snackbarContent =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Paper.PaperPropsRow", divProps ]
    --    , name: "SnackbarContent"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ Tuple "action" jsx
    --              , Tuple "message" jsx
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "role"
    --        ]
    --      }
    --    }

    --step =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "Step"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable [ children ]
    --      , generate:
    --        [ "active"
    --        , "classes"
    --        , "completed"
    --        , "disabled"
    --        , "expanded"
    --        ]
    --      }
    --    }

    --stepButton =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsRow"
    --        , Type.constructor "MUI.DOM.Generated.Props_button"
    --        ]
    --    , name: "StepButton"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "icon" foreignType
    --              , Tuple "optional" jsx
    --              ]
    --      , generate:
    --        [ "active"
    --        , "alternativeLabel"
    --        , "classes"
    --        , "completed"
    --        , "disabled"
    --        , "last"
    --        , "orientation"
    --        ]
    --      }
    --    }

    --stepConnector =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "StepConnector"
    --    , propsRow:
    --      { base: emptyBase
    --      , generate: [ "classes" ]
    --      }
    --    }

    ---- | TODO: handle TransitionComponent
    --stepContent =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "StepContent"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "transitionDuration"
    --        -- , "TransitionComponent"
    --        , "TransitionProps"
    --        ]
    --      }
    --    }

    --stepIcon =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "StepIcon"
    --    , propsRow:
    --      { base:
    --            Map.fromFoldable
    --                [ Tuple "icon" jsx
    --                ]
    --      , generate:
    --        [ "active"
    --        , "classes"
    --        , "completed"
    --        , "error"
    --        ]
    --      }
    --    }

    ---- | TODO: StepIconComponent
    --stepLabel =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "StepLabel"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "icon" jsx
    --              , Tuple "optional" jsx
    --              , Tuple "StepIconComponent" foreignType
    --              , Tuple "StepIconProps" (Type.constructor "MUI.Core.StepIcon.StepIconOpaqueProps")
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "disabled"
    --        , "error"
    --        ]
    --      }
    --    }

    --stepper =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Paper.PaperPropsRow"
    --        , divProps
    --        ]
    --    , name: "Stepper"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "connector" jsx
    --              ]
    --      , generate:
    --        [ "activeStep"
    --        , "alternativeLabel"
    --        , "classes"
    --        , "nonLinear"
    --        , "orientation"
    --        ]
    --      }
    --    }

    svgIcon = simpleComponent
      { name: "SvgIcon"
      , propsRow:
        { base:
            Map.fromFoldable
                [ children
                ]
        , generate:
          [ "classes"
          , "color"
          , "fontSize"
          , "htmlColor"
          , "shapeRendering"
          , "titleAccess"
          , "viewBox"
          ]
        }
      , root: rbProps.svg
      }

    --swipeableDrawer =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Drawer.DrawerPropsRow"
    --        , divProps
    --        ]
    --    , name: "SwipeableDrawer"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "SwipeAreaProps" foreignType
    --              , eventHandlerProp "onClose"
    --              , eventHandlerProp "onOpen"
    --              ]
    --      , generate:
    --        [ "disableBackdropTransition"
    --        , "disableDiscovery"
    --        , "disableSwipeToOpen"
    --        , "hysteresis"
    --        , "minFlingVelocity"
    --        , "open"
    --        , "swipeAreaWidth"
    --        ]
    --      }
    --    }

    --switch =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.IconButton.IconButtonPropsRow"
    --        , divProps
    --        ]
    --    , name: "Switch"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ Tuple "checkedIcon" jsx
    --              , Tuple "icon" jsx
    --              , Tuple "inputProps" foreignType
    --              , Tuple "inputRef" foreignType
    --              , eventHandlerProp "onChange"
    --              , Tuple "value" foreignType
    --              ]
    --      , generate:
    --        [ "checked"
    --        , "classes"
    --        , "color"
    --        , "disabled"
    --        , "disableRipple"
    --        , "edge"
    --        , "id"
    --        , "required"
    --        , "size"
    --        , "type"
    --        ]
    --      }
    --    }

    --tab =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsRow"
    --        , Type.constructor "MUI.DOM.Generated.Props_button"
    --        ]
    --    , name: "Tab"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "icon" jsx
    --              , Tuple "label" jsx
    --              , Tuple "value" foreignType
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "disabled"
    --        , "disableFocusRipple"
    --        , "disableRipple"
    --        , "fullWidth"
    --        , "selected"
    --        , "wrapped"
    --        ]
    --      }
    --    }

    --table =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_table" ]
    --    , name: "Table"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "padding"
    --        , "size"
    --        , "stickyHeader"
    --        ]
    --      }
    --    }

    --tableBody =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_tbody" ]
    --    , name: "TableBody"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "classes"
    --        ]
    --      }
    --    }

    --tableCell =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_td" ]
    --    , name: "TableCell"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "padding"
    --        , "scope"
    --        , "size"
    --        --, "scopeDirection"
    --        , "variant"
    --        ]
    --      }
    --    }

    --tableFooter =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_tfoot" ]
    --    , name: "TableFooter"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "classes"
    --        ]
    --      }
    --    }

    --tableHead =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_thead" ]
    --    , name: "TableHead"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "classes"
    --        ]
    --      }
    --    }

    ---- | TODO: add TablePaginationActions
    --tablePagination :: Component
    --tablePagination =
    --  { extraDeclarations: []
    --  , inherits: Just $ MUI.rList
    --      [ Type.constructor "MUI.Core.TableCell.TableCellPropsRow"
    --      , Type.constructor "MUI.DOM.Generated.Props_td"
    --      ]
    --  , modulePath: Name "TablePagination"
    --  , propsRow:
    --    { base:
    --        Map.fromFoldable
    --            [ children
    --            , Tuple "backIconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonOpaqueProps")
    --            , Tuple "labelDisplayedRows" foreignType
    --            , Tuple "labelRowsPerPage" jsx
    --            , Tuple "nextIconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonOpaqueProps")
    --            , eventHandlerProp "onChangePage"
    --            , eventHandlerProp "onChangeRowsPerPage"
    --            , Tuple "SelectProps" (Type.constructor "MUI.Core.Select.SelectOpaqueProps")
    --            ]
    --    , generate:
    --      [ "classes"
    --      , "count"
    --      , "page"
    --      , "rowsPerPage"
    --      , "rowsPerPageOptions"
    --      ]
    --    -- | Long story short `TablePaginationProps` is a union
    --    -- | so we are not able to create an interface instance for it.
    --    -- | Fortunately we are able to extract interesting props
    --    -- | from the first part component of this union...
    --    , instantiation:
    --      Just
    --        { strategy: TypeAlias
    --        , extractProps:
    --          \defaultInstance -> case unroll defaultInstance of
    --            (Instantiation.Union [ Mu.In (Instantiation.Object fqn props), _ ]) -> pure { fqn, props }
    --            otherwise ->
    --              throwError
    --                [ "Expecting an union as a representation for TablePaginationOpaqueProps" ]
    --        }
    --    }
    --  , tsc: { strictNullChecks: false }
    --  }

    --tableRow =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_tr" ]
    --    , name: "TableRow"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "hover"
    --        , "selected"
    --        ]
    --      }
    --    }

    ---- | TODO: IconComponent
    --tableSortLabel =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsRow"
    --        , Type.constructor "MUI.DOM.Generated.Props_button"
    --        ]
    --    , name: "TableSortLabel"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "IconComponent" foreignType
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "active"
    --        , "direction"
    --        , "hideSortIcon"
    --        ]
    --      }
    --    }

    --tabs =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "Tabs"
    --    , propsRow:
    --      { base: Map.fromFoldable
    --        [ children
    --        , Tuple "action" foreignType
    --        , eventHandlerProp "onChange"
    --        , Tuple "ScrollButtonComponent" foreignType
    --        , Tuple "TabIndicatorProps" foreignType
    --        , Tuple "value" foreignType
    --        ]
    --      , generate:
    --        [ "centered"
    --        , "classes"
    --        , "indicatorColor"
    --        , "orientation"
    --        , "scrollButtons"
    --        , "textColor"
    --        , "variant"
    --        , "width"
    --        ]
    --      }
    --    }

    --textareaAutosize =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_textarea" ]
    --    , name: "TextareaAutosize"
    --    , propsRow:
    --      { base: emptyBase
    --      , generate:
    --        [ "rows"
    --        , "rowsMax"
    --        ]
    --      }
    --    }

    textField textFieldType unionMember =
      { extraDeclarations: []
      , modulePath:
        { input: Name "TextField"
        , output: Path "TextField" (Name textFieldType)
        }
      , propsRow:
        { base: Map.fromFoldable
            [ children
            , Tuple "defaultValue" foreignType
            , Tuple "helperText" jsx
            --, Tuple "InputLabelProps" (Type.constructor "MUI.Core.InputLabel.InputLabelOpaqueProps")
            -- , Tuple "inputProps" foreignType
            -- , Tuple "inputRef" foreignType
            --, Tuple "FormHelperTextProps" (Type.constructor "MUI.Core.FormHelperText.FormHelperTextOpaqueProps")
            , Tuple "label" jsx
            , eventHandlerProp "onChange"
            , eventHandlerProp "onBlur"
            , eventHandlerProp "onFocus"
            --, Tuple "SelectProps" (Type.constructor "MUI.Core.Select.SelectOpaqueProps")
            , Tuple "value" foreignType
            ]
        , generate:
            [ "autoComplete"
            , "autoFocus"
            , "classes"
            , "color"
            , "disabled"
            , "error"
            , "fullWidth"
            , "id"
            , "margin"
            , "multiline"
            , "name"
            , "placeholder"
            , "required"
            , "rows"
            , "rowsMax"
            , "select"
            , "type"
            , "variant"
            ]
        , ts:
            { instantiation: Just
                { strategy: TypeAlias
                , extractProps: \defaultInstance -> case unroll defaultInstance of
                    ( Instantiation.Union
                        [ Mu.In (Instantiation.Object fqn1 props1)
                        , Mu.In (Instantiation.Object fqn2 props2)
                        , Mu.In (Instantiation.Object fqn3 props3)
                        ]
                    ) -> case unionMember of
                      1 → pure { fqn: fqn1, props: props1 }
                      2 → pure { fqn: fqn2, props: props2 }
                      otherwise → pure { fqn: fqn3, props: props3 }
                    --  throwError
                    --    [ "Not sure how to tackle this three props types: " <> fqn1 <> ", " <> fqn2 <> "," <> fqn3 ]
                    (Instantiation.Union _) ->
                      throwError
                        [ "Expecting a two member union as a representation for TextField" ]
                    i ->
                      throwError
                        [ "Expecting an union as a representation for TextField got: " <> unsafeStringify i ]
                }
            , unionName: \_ _ → Nothing
            }
        }
      , root: MUIComponent formControl
      }

    ---- | It seems that toggle button is still in `material-ui-lab`
    --toggleButton =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsRow"
    --        , Type.constructor "MUI.DOM.Generated.Props_button"
    --        ]
    --    , name: "ToggleButton"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , Tuple "value" foreignType
    --              ]
    --      , generate:
    --        [ "classes"
    --        , "disabled"
    --        , "disableFocusRipple"
    --        , "disableRipple"
    --        , "selected"
    --        ]
    --      }
    --    }

    toolbar =
      simpleComponent
        { name: "Toolbar"
        , propsRow:
          { base:
              Map.fromFoldable
                  [ children
                  ]
          , generate:
            [ "classes"
            , "disableGutters"
            , "variant"
            ]
          }
        , root: rbProps.div
        }

    --touchRipple =
    --  { extraDeclarations: []
    --  , inherits: Just $ MUI.rList' [ "MUI.DOM.Generated.Props_span" ]
    --  -- , name: touchRippleType.name
    --  , modulePath: touchRippleType.path
    --  , propsRow:
    --    { base: emptyBase
    --    , generate: [ "center", "classes" ]
    --    , instantiation: Nothing
    --    }
    --  , tsc: { strictNullChecks: false }
    --  }

    -- | Fortunatelly Props_p and Props_h*
    -- | have the same set of properties
    -- | so we can just use one of them as
    -- | a root.
    typography =
      simpleComponent
        { name: "Typography"
        , propsRow:
          { base: Map.fromFoldable [ children ]
          , generate:
            [ "classes"
            , "align"
            , "color"
            , "display"
            , "gutterBottom"
            , "noWrap"
            , "paragraph"
            , "variant"
            , "variantMapping"
            ]
          }
        , root: rbProps.p
        }

    ---- | TODO: needs to extend Transition
    --zoom =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "Zoom"
    --    , propsRow:
    --      { base: emptyBase
    --      , generate: [ "in", "timeout" ]
    --      }
    --    }
  in
    [ appBar
    -- , avatar
    -- , backdrop
    , badge
    -- , bottomNavigation
    -- , bottomNavigationAction
    -- , box
    -- , breadcrumbs
    , buttonBase
    , buttonGroup
    , button
    -- , card
    -- , cardActionArea
    -- , cardActions
    -- , cardContent
    -- , cardHeader
    -- , cardMedia
    -- , circularProgress
    -- , clickAwayListener
    -- , checkbox
    -- , chip
    -- , collapse
    , container
    -- , cssBaseline
    -- , dialog
    -- , dialogActions
    -- , dialogContent
    -- , dialogTitle
    , divider
    -- , drawer
    -- , expansionPanel
    -- , expansionPanelActions
    -- , expansionPanelDetails
    -- , expansionPanelSummary
    -- , fab
    -- , fade
    , formControl
    , formControlLabel
    , formGroup
    , formHelperText
    , formLabel
    , grid
    -- , gridList
    -- , gridListTile
    -- , gridListTileBar
    -- , grow
    , hidden
    -- , icon
    , iconButton
    , input
    -- , inputAdornment
    , inputBase
    , inputLabel
    -- , outlineInput
    -- , linearProgress
    , link
    -- , list
    -- , listItem
    -- , listItemAvatar
    -- , listItemIcon
    -- , listItemSecondaryAction
    -- , listItemText
    -- , listSubheader
    -- , menu
    -- , menuItem
    -- , modal
    -- , nativeSelect
    -- , noSsr
    , paper
    -- , popover
    -- , popper
    -- , portal
    -- , radio
    -- , radioGroup
    -- , rootRef
    -- , select
    -- , slide
    -- , slider
    -- , snackbar
    -- , snackbarContent
    -- , step
    -- , stepButton
    -- , stepConnector
    -- , stepContent
    -- , stepIcon
    -- , stepLabel
    -- , stepper
    , svgIcon
    -- , swipeableDrawer
    -- , switch
    -- , tab
    -- , table
    -- , tableBody
    -- , tableCell
    -- , tableFooter
    -- , tableHead
    -- , tablePagination
    -- , tableRow
    -- , tableSortLabel
    -- , tabs
    -- , textareaAutosize
    , (textField "StandardTextField" 1)
    , (textField "FilledTextField" 2)
    , (textField "OutlinedTextField" 3)
    -- -- , toggleButton
    , toolbar
    -- , touchRipple
    , typography
    -- , zoom
    ]

-- | XXX: Can we cleanup this last traverse?
multiString :: ∀ a. Pattern -> ReadM a -> ReadM (Array a)
multiString splitPattern read = do
  s <- readerAsk
  elems <-
    let
      strArray = filter (not <<< String.null) $ split splitPattern s
    in
      if Array.null strArray then
        readerError "Got empty string as input"
      else
        pure strArray
  let
    read' = unwrap read
  wrap $ for elems \elem -> lift $ runReaderT read' elem

componentOption :: Parser Component
componentOption = option componentRead (long "component" <> short 'c')

componentRead :: ReadM Component
componentRead =
  eitherReader
    $ \s -> case s `Map.lookup` components' of
        Just c -> pure c
        otherwise ->
          Left
            $ intercalate " "
                [ "Unkown component name"
                , show s
                , ". Please use one of:"
                , intercalate ", " (map show <<< List.sort <<< Map.Internal.keys $ components') <> "."
                ]
  where
  step c = Tuple (Component.componentName c) c

  components' = Map.fromFoldable $ map step components

iconOption :: Parser Icon
iconOption = option iconRead (long "icon" <> short 'i')

iconRead :: ReadM Icon
iconRead =
  eitherReader
    $ \s -> case s `Map.lookup` icons' of
        Just c -> pure c
        otherwise ->
          Left
            $ intercalate " "
                [ "Unkown icon name"
                , show s
                , ". Please use one of:"
                , intercalate ", " (List.sort <<< Map.Internal.keys $ icons') <> "."
                ]
  where
  step i = Tuple (iconName i) i

  icons' = Map.fromFoldable $ map step icons

commaSeparatedComponentList :: { helpText :: String, long ∷ String, short ∷ Char } -> Parser (Array Component)
commaSeparatedComponentList { helpText, long: l, short: s } =
  option (multiString (Pattern ",") componentRead)
    ( long l
        <> short s
        <> metavar "component1,component2,...,component3"
        <> help helpText
        <> value []
    )

data GenOutput
  = Directory FilePath
  | Stdout

genOutput :: Parser GenOutput
genOutput = directory <|> stdout
  where
  directory =
    map Directory $ strOption
      $ long "directory"
      <> short 'd'
      <> metavar "DIRECTORY"
      <> help "Write output components to directory"

  stdout =
    flag' Stdout
      $ long "stdout"
      <> short 's'
      <> help "Print component to stdout"

data GenTarget
  = GenAllComponents
  | GenComponents (Array Component)
  -- | GenIcons
  | GenIcon Icon

genTarget :: Parser GenTarget
genTarget = genComponents <|> genAllComponents <|> genIcon
  where
  genComponents = GenComponents <$> commaSeparatedComponentList { helpText, long: "components", short: 'c' }
    where
    helpText = "A comma-separated list of component names."

  genAllComponents =
    flag' GenAllComponents
      $ long "all-components"
      <> short 'a'
      <> help "Codegen all components"

  genIcon = map GenIcon iconOption

data Options
  = ShowPropsCommand
    { component :: Component
    , skip :: Array Component
    }
  | GenerateCommand
    { target :: GenTarget
    , output :: Maybe GenOutput
    }

genOptions :: Parser Options
genOptions =
  map GenerateCommand $ { target: _, output: _ }
    <$> genTarget
    <*> (Just <$> genOutput <|> pure Nothing)

showPropsOptions :: Parser Options
showPropsOptions =
  map ShowPropsCommand $ { component: _, skip: _ }
    <$> componentOption
    <*> commaSeparatedComponentList { helpText, long: "skip-props", short: 's' }
  where
  helpText =
    intercalate "."
      [ "A comma-separated list of component names "
      , "which props should be dropped from props list."
      , "This can be useful if you want to list only component own "
      , "properties or destil some inheritance etc."
      ]

options :: Parser Options
options =
  subparser
    $ command "codegen" (info (genOptions <**> helper) (progDesc "Codegen all or a given module"))
    <> command "show-props" (info (showPropsOptions <**> helper) (progDesc "Show information about props for a given component"))

main :: Effect Unit
main = do
  opts <- execParser (info (options <**> helper) fullDesc)
  let
    -- | Currently we use global import aliasing setup which works quite well.
    importAliases = Map.fromFoldable $
      [ ModuleName "Unsafe.Coerce" /\ Nothing
      , ModuleName "Unsafe.Reference" /\ Nothing
      , ModuleName "MUI.Core" /\ Nothing
      , ModuleName "React.Basic" /\ Nothing
      , ModuleName "MUI.DOM.Generated" /\ Just "DOM"
      ]

    -- | Should we introduce multimodule handling logic back?
    -- | For sure we want to keep track of the naming collisions
    -- | during codegen so may we can just require that
    -- | we would have an unique naming strategy.
    writeComponentModules dir component code =
      launchAff_
        $ do
            let
              js = Codegen (componentJSFile component) code.js

              ps = Codegen (componentPSFile component) code.ps
            Codegen.write dir js
            Codegen.write dir ps

    codegenComponent component output =
      runReaderT (runExceptT (Codegen.component component)) importAliases
        >>= \c → do
          log $ "Generating module:" <> show component.modulePath
          case c of
            Right code -> case output of
              Just Stdout -> do
                log "\nPureScript:"
                log code.ps
                log "\nJavaScript:"
                log code.js
              Just (Directory d) -> do
                writeComponentModules d component code
              Nothing -> do
                writeComponentModules "./src" component code
            Left err -> do
              log $ "\n"
                <> (psImportPath $ component.modulePath.output)
                <> " component codegen errors: "
                <> intercalate "\n" err

    writeIconModules dir icon code =
      launchAff_
        $ do
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
        writeIconModules "./src" icon code
      where
        code = runReader (Codegen.icon icon) importAliases
  case opts of
    ShowPropsCommand { component, skip } -> do
      let
        getProps = do
          { props: c } <- TS.MUI.componentProps component
          s <- traverse (\s -> TS.MUI.componentProps s) skip <#> map _.props
          pure { component: c, skip: s }
      runReaderT (runExceptT getProps) mempty
        >>= case _ of
            Right { component: cProps, skip: s } -> do
              let
                cKeys = Map.keys cProps

                sKeys = Set.unions (map Map.keys s)

                keys = cKeys `Set.difference` sKeys

                props =
                  Array.sort
                    $ map (\(Tuple k v) -> k <> " : " <> cata pprintTypeName v.type)
                    $ Map.toUnfoldable
                    $ Map.filterWithKey (\k v -> k `Set.member` keys) cProps
              log $ intercalate "\n" props
            Left err -> log $ intercalate "\n" err
    GenerateCommand { target: GenComponents components', output } ->
      for_ components' \component ->
        codegenComponent component output
    GenerateCommand { target: GenAllComponents, output } ->
      for_ components \component ->
        codegenComponent component output
    GenerateCommand { target: GenIcon i, output } -> iconCodegen i output
