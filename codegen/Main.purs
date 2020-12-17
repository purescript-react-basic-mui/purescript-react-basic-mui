module Codegen.Main where

import Prelude
import Codegen (Codegen(..), componentJSFile, componentPSFile, iconJSFile, iconPSFile, icons)
import Codegen (component, icon, write) as Codegen
import Codegen.AST (ModuleName(..), TypeName(..))
import Codegen.AST (Type) as AST
import Codegen.AST.Sugar.Type (boolean, int, number, string)
import Codegen.AST.Sugar.Type (constructor) as Type
import Codegen.AST.Types (TypeF(..))
import Codegen.Component (Component, FieldDetails, Icon, ModulePath(..), Root(..), arrayJSX, iconName, jsx, psImportPath, rbProps)
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
import Data.Functor.Mu (roll, unroll)
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

checkedProp :: String -> AST.Type -> Tuple String { force :: Maybe FieldDetails, type :: AST.Type }
checkedProp l t = Tuple l { force: Nothing, "type": t }

forcedProp :: String -> AST.Type -> Boolean -> Tuple String { force :: Maybe FieldDetails, type :: AST.Type }
forcedProp l t r = Tuple l { force: Just { required: r }, "type": t }

components :: Array Component
components =
  let
    aria =
      [ "aria-haspopup"
      , "aria-controls"
      ]

    children = checkedProp "children" arrayJSX

    forcedChildren = forcedProp "children" arrayJSX true

    eventHandlerProp name = checkedProp name (Type.constructor "React.Basic.Events.EventHandler")

    foreignType = Type.constructor "Foreign.Foreign"

    jss = Type.constructor "MUI.Core.JSS"

    flexbox =
      [ checkedProp "alignContent" (Type.constructor "MUI.System.Flexbox.AlignContent")
      , checkedProp "alignItems" (Type.constructor "MUI.System.Flexbox.AlignItems")
      , checkedProp "alignSelf" (Type.constructor "MUI.System.Flexbox.AlignSelf")
      , checkedProp "flexDirection" (Type.constructor "MUI.System.Flexbox.FlexDirection")
      , checkedProp "flexGrow" number
      , checkedProp "flexBasis" string
      , checkedProp "flexShrink" number
      , checkedProp "flexWrap" (Type.constructor "MUI.System.Flexbox.FlexWrap")
      , checkedProp "justifyContent" (Type.constructor "MUI.System.Flexbox.JustifyContent")
      ]

    transitionTimeout = Type.constructor "MUI.React.TransitionGroup.Timeout"

    -- | TODO: handle all transition group props
    -- | https://reactcommunity.org/react-transition-group/transition#Transition-props
    transitionHandlers =
      map eventHandlerProp
        [ "onEnter"
        , "onEntered"
        , "onEntering"
        , "onExit"
        , "onExited"
        , "onExiting"
        ]

    -- | It is nice to have parametrized `Row` by default
    -- | because it allows us
    -- basePropsRow props = props
    -- emptyBase = basePropsRow mempty
    simpleComponent { name, propsRow: { base, generate }, root } =
      { extraDeclarations: []
      , modulePath:
          { input: Path "core" (Name name)
          , output: Path "Core" (Name name)
          }
      , propsRow:
          { base
          , generate: generate <> aria
          , ts: { instantiation: Nothing, unionName: \_ _ -> Nothing }
          }
      , root
      }

    touchRippleType =
      { path: (Path "Core" (Path "ButtonBase" (Name "TouchRipple")))
      , name: "TouchRipple"
      }

    appBar =
      simpleComponent
        { name: "AppBar"
        , propsRow:
            { base: Map.fromFoldable [ children ]
            , generate: [ "classes", "color", "elevation", "position" ]
            }
        , root: MUIComponent paper
        }

    avatar =
      simpleComponent
        { name: "Avatar"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  , checkedProp "imgProps" foreignType
                  ]
            , generate:
                [ "alt"
                , "classes"
                , "sizes"
                , "src"
                , "srcSet"
                , "variant"
                ]
            }
        , root: rbProps.div
        }

    backdrop =
      simpleComponent
        { name: "Backdrop"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  , checkedProp "transitionDuration" transitionTimeout
                  ]
            , generate: [ "classes", "invisible", "open" ]
            }
        , root: rbProps.div
        }

    badge =
      simpleComponent
        { root: rbProps.div
        , name: "Badge"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "badgeContent" jsx
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
    box =
      let
        sizing =
          [ checkedProp "boxSizing" (Type.constructor "MUI.System.BoxSizing")
          , checkedProp "height" string
          , checkedProp "maxHeight" string
          , checkedProp "minHeight" string
          , checkedProp "maxWidth" string
          , checkedProp "minWidth" string
          , checkedProp "width" string
          ]

        -- | This is based on `const spacing: SimpleStyleFunction<...>` from
        -- | node_modules/@material-ui/system/index.d.ts
        -- |
        -- | It seems that this typescript type is only used by Box
        spacing =
          [ checkedProp "m" int
          , checkedProp "mt" int
          , checkedProp "mb" int
          , checkedProp "ml" int
          , checkedProp "mr" int
          , checkedProp "mx" int
          , checkedProp "my" int
          , checkedProp "p" int
          , checkedProp "pt" int
          , checkedProp "pb" int
          , checkedProp "pl" int
          , checkedProp "pr" int
          , checkedProp "px" int
          , checkedProp "py" int
          , checkedProp "margin" int
          , checkedProp "marginTop" int
          , checkedProp "marginRight" int
          , checkedProp "marginBottom" int
          , checkedProp "marginLeft" int
          , checkedProp "marginX" int
          , checkedProp "marginY" int
          , checkedProp "padding" int
          , checkedProp "paddingTop" int
          , checkedProp "paddingRight" int
          , checkedProp "paddingBottom" int
          , checkedProp "paddingLeft" int
          , checkedProp "paddingX" int
          , checkedProp "paddingY" int
          ]
      in
        simpleComponent
          { name: "Box"
          , propsRow:
              { base:
                  Map.fromFoldable
                    $ flexbox
                    <> sizing
                    <> spacing
                    <> [ children
                      , checkedProp "border" foreignType
                      , checkedProp "borderBottom" foreignType
                      , checkedProp "borderColor" foreignType
                      , checkedProp "borderLeft" foreignType
                      , checkedProp "borderRadius" foreignType
                      , checkedProp "borderRight" foreignType
                      , checkedProp "borderTop" foreignType
                      , checkedProp "component" (roll TypeString)
                      , checkedProp
                          "display"
                          (Type.constructor "MUI.System.Display.Display")
                      ]
              , generate:
                  [ "clone"
                  ]
              }
          , root: rbProps.div
          }

    breadcrumbs =
      simpleComponent
        { name: "Breadcrumbs"
        , propsRow:
            { base:
                Map.fromFoldable
                  $ [ children
                    , checkedProp "ref" foreignType
                    , forcedProp "component" foreignType false
                    , checkedProp "separator" jsx
                    ]
            , generate: [ "classes", "expandText", "itemsAfterCollapse", "itemsBeforeCollapse", "maxItems" ]
            }
        , root: rbProps.nav
        }

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
          { input: Path "core" (Name "ButtonBase")
          , output: Path "Core" (Name "ButtonBase")
          }
      , propsRow:
          { base:
              Map.fromFoldable
                [ checkedProp "action" foreignType
                , checkedProp "buttonRef" foreignType
                , eventHandlerProp "onFocusVisible"
                -- | I'm not sure hot to handle this kind of props parameter
                -- | in the current architecture.
                -- , checkedProp "TouchRippleProps" $ Type.recordLiteral $ Type.app
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
          , ts: { instantiation: Nothing, unionName: \_ _ -> Nothing }
          }
      , root: rbProps.button
      }

    button =
      simpleComponent
        { name: "Button"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "endIcon" jsx
                  , checkedProp "startIcon" jsx
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

    buttonGroup =
      simpleComponent
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
    --          [ checkedProp "icon" jsx
    --          , checkedProp "label" jsx
    --          ]
    --      , generate:
    --        [ "classes"
    --        , "showLabel"
    --        , "selected"
    --        ]
    --      }
    --    }
    card =
      simpleComponent
        { name: "Card"
        , propsRow:
            { base: Map.fromFoldable [ children ]
            , generate: [ "classes", "raised" ]
            }
        , root: MUIComponent paper
        }

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
    cardActions =
      simpleComponent
        { name: "CardActions"
        , propsRow:
            { base: Map.fromFoldable [ children ]
            , generate: [ "classes", "disableSpacing" ]
            }
        , root: rbProps.div
        }

    --cardContent =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "CardContent"
    --    , propsRow:
    --      { base:  Map.fromFoldable [ children ]
    --      , generate: [ "classes" ]
    --      }
    --    }
    cardHeader =
      simpleComponent
        { name: "CardHeader"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "action" jsx
                  , checkedProp "avatar" jsx
                  , children
                  , checkedProp "subheader" jsx
                  , checkedProp "subheaderTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyProps")
                  , checkedProp "title" jsx
                  , checkedProp "titleTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyProps")
                  ]
            , generate: [ "classes", "disableTypography" ]
            }
        , root: rbProps.div
        }

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
    checkbox =
      simpleComponent
        { name: "Checkbox"
        , propsRow:
            { base:
                Map.fromFoldable
                  ( [ checkedProp "ref" foreignType
                    , checkedProp "checkedIcon" jsx
                    , checkedProp "icon" jsx
                    , checkedProp "indeterminateIcon" jsx
                    , checkedProp "inputProps" foreignType
                    , checkedProp "inputRef" foreignType
                    , checkedProp "value" string
                    ]
                      <> (map eventHandlerProp [ "onChange" ])
                  )
            , generate:
                [ "checked"
                , "classes"
                , "color"
                , "disabled"
                , "disableRipple"
                , "id"
                , "indeterminate"
                , "required"
                , "size"
                ]
            }
        , root: MUIComponent iconButton
        }

    --chip =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList [ divProps ]
    --    , name: "Chip"
    --    , propsRow:
    --      { base:  Map.fromFoldable
    --        [ checkedProp "avatar" jsx
    --        , checkedProp "deleteIcon" jsx
    --        , checkedProp "icon" jsx
    --        , checkedProp "label" jsx
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
    circularProgress =
      simpleComponent
        { name: "CircularProgress"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  ]
            , generate:
                [ "classes"
                , "color"
                , "disableShrink"
                , "size"
                , "thickness"
                , "value"
                , "variant"
                ]
            }
        , root: rbProps.div
        }

    --clickAwayListener =
    --  let
    --    onClickAway = eventHandlerProp "onClickAway"
    --    -- | Single jsx node is required
    --    child = checkedProp "children" jsx
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
    collapse =
      simpleComponent
        { name: "Collapse"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ children
                  , checkedProp "ref" foreignType
                  , forcedProp "component" (roll TypeString) false
                  ]
            , generate:
                [ "classes"
                , "collapsedHeight"
                , "disableStrictModeCompat"
                , "in"
                , "timeout"
                ]
            }
        --   TODO: should be something like this once TransitionGroup stuff is defined:
        -- , root: MUIComponent transition
        , root: rbProps.div
        }

    container =
      simpleComponent
        { name: "Container"
        , propsRow:
            { base: mempty
            , generate:
                [ "classes"
                , "disableGutters"
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
    dialog =
      let
        -- | TODO:
        -- | migration.
        -- | * `PaperComponent`, `PaperProps`, `TransitionComponent`, `TransitionDuration`
        handlers =
          map eventHandlerProp
            [ "onBackdropClick"
            , "onClose"
            , "onEnter"
            , "onEntered"
            , "onEntering"
            , "onEscapeKeyDown"
            , "onExit"
            , "onExited"
            , "onExiting"
            ]

        base =
          Map.fromFoldable
            $ [ checkedProp "ref" foreignType
              , children
              , checkedProp "PaperComponent" foreignType
              , checkedProp "PaperProps" (Type.constructor "MUI.Core.Paper.PaperProps")
              , checkedProp "TransitionComponent" foreignType
              , checkedProp "transitionDuration" transitionTimeout
              , checkedProp "TransitionProps" foreignType
              ]
            <> handlers
      in
        simpleComponent
          { name: "Dialog"
          , propsRow:
              { base
              , generate:
                  [ "aria-describedby"
                  , "aria-labelledby"
                  , "classes"
                  , "disableBackdropClick"
                  , "disableEscapeKeyDown"
                  , "fullScreen"
                  , "fullWidth"
                  , "maxWidth"
                  , "open"
                  , "scroll"
                  ]
              }
          , root: rbProps.div
          }

    dialogActions =
      simpleComponent
        { name: "DialogActions"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ children
                  , checkedProp "ref" foreignType
                  ]
            , generate: [ "classes", "disableSpacing" ]
            }
        , root: rbProps.div
        }

    dialogContent =
      simpleComponent
        { name: "DialogContent"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ children
                  , checkedProp "ref" foreignType
                  ]
            , generate: [ "classes", "dividers" ]
            }
        , root: rbProps.div
        }

    dialogContentText =
      simpleComponent
        { name: "DialogContentText"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ children
                  , checkedProp "ref" foreignType
                  ]
            , generate: [ "classes" ]
            }
        , root: MUIComponent typography
        }

    dialogTitle =
      simpleComponent
        { name: "DialogTitle"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ children
                  , checkedProp "ref" foreignType
                  ]
            , generate: [ "classes", "disableTypography" ]
            }
        , root: rbProps.div
        }

    ---- | TODO: add component
    divider =
      simpleComponent
        { name: "Divider"
        , propsRow:
            { base: Map.fromFoldable []
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

    drawer =
      simpleComponent
        { name: "Drawer"
        , propsRow:
            { base:
                Map.fromFoldable
                  ( [ children
                    , checkedProp "ModalProps" (Type.constructor "MUI.Core.Modal.ModalProps")
                    , checkedProp "PaperProps" (Type.constructor "MUI.Core.Paper.PaperProps")
                    -- , checkedProp "SlideProps" (Type.constructor "MUI.Core.Slide.SlideOpaqueProps")
                    , checkedProp "transitionDuration" transitionTimeout
                    ]
                      <> [ eventHandlerProp "onClose" ]
                      <> transitionHandlers
                  )
            , generate:
                [ "anchor"
                , "classes"
                , "elevation"
                , "open"
                , "variant"
                ]
            }
        , root: rbProps.div
        }

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
    --          , checkedProp "expandIcon" jsx
    --          , checkedProp "IconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonOpaqueProps")
    --          ]
    --      , generate: [ "classes" ]
    --      }
    --    }
    fab =
      simpleComponent
        { name: "Fab"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ children
                  , checkedProp "ref" foreignType
                  , forcedProp "component" foreignType false
                  ]
            , generate:
                [ "classes"
                , "color"
                , "disabled"
                , "disableFocusRipple"
                , "disableRipple"
                , "href"
                , "size"
                , "variant"
                ]
            }
        , root: MUIComponent buttonBase
        }

    ---- | TODO: TransitionComponent
    fade =
      simpleComponent
        { name: "Fade"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "children" jsx
                  , checkedProp "ref" foreignType
                  , checkedProp "timeout" transitionTimeout
                  ]
            , generate: [ "in" ]
            }
        , root: rbProps.div
        }

    ---- | TODO: inputComponent, make value a type variable
    --filledInput =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList' [ "MUI.Core.InputBasePropsRow" ]
    --    , name: "FilledInput"
    --    , propsRow:
    --      { base:  Map.fromFoldable
    --          [ children
    --          , checkedProp "endAdornment" jsx
    --          , checkedProp
    --              "inputProps"
    --              (Type.constructor "MUI.Core.InputBaseOpaqueProps")
    --          , checkedProp "inputRef" foreignType
    --          , eventHandlerProp "onChange"
    --          , checkedProp "startAdornment" jsx
    --          , checkedProp "value" foreignType
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
            { base:
                Map.fromFoldable
                  [ checkedProp "control" jsx
                  , checkedProp "label" jsx
                  , eventHandlerProp "onChange"
                  , checkedProp "value" string
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
          { input: Path "core" $ Name "Grid"
          , output: Path "Core" $ Name "Grid"
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
              , unionName:
                  \property members -> case property of
                    _
                      | property `Array.elem` [ "xs", "sm", "md", "lg", "xl" ] -> Just $ TypeName "GridSize"
                    "spacing" -> Just $ TypeName "GridSpacing"
                    "justify" -> Just $ TypeName "GridJustification"
                    otherwise -> Nothing
              }
          }
      , root: rbProps.div
      }

    gridList =
      simpleComponent
        { name: "GridList"
        , propsRow:
            { base: Map.fromFoldable [ children ]
            , generate: [ "cellHeight", "classes", "cols", "spacing" ]
            }
        , root: rbProps.ul
        }

    gridListTile =
      simpleComponent
        { name: "GridListTile"
        , propsRow:
            { base: Map.fromFoldable [ children ]
            , generate: [ "classes", "cols", "rows" ]
            }
        , root: rbProps.ul
        }

    --gridListTileBar =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "GridListTileBar"
    --    , propsRow:
    --      { base: Map.fromFoldable
    --          [ checkedProp "actionIcon" jsx
    --          , checkedProp "subtitle" jsx
    --          , checkedProp "title" jsx
    --          ]
    --      , generate:
    --        [ "classes"
    --        , "actionPosition"
    --        , "titlePosition"
    --        ]
    --      }
    --    }
    ---- | TODO: update when Transition is figured out
    grow =
      simpleComponent
        { root: rbProps.div
        , name: "Grow"
        , propsRow:
            { base: mempty
            , generate: [ "in", "timeout" ]
            }
        }

    -- | It seems that on the current master
    -- | `HiddenCss` uses `div` as a root
    -- | but `HiddenJs` returns just children.
    -- |
    -- | It seems that hidden could be also removed in the near future:
    -- |
    -- | https://github.com/mui-org/material-ui/issues/19704
    -- |
    -- | In other words root will be ignored when they
    -- | are provided to the component.
    hidden =
      { extraDeclarations: []
      , modulePath:
          { input: Path "core" $ Name "Hidden"
          , output: Path "Core" $ Name "Hidden"
          }
      , propsRow:
          { base:
              Map.fromFoldable
                -- | It seems that there is a bug in Hidden typing
                [ forcedChildren ]
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
              , unionName:
                  case _ of
                    "Anonymous" -> const $ Just $ TypeName "Only"
                    otherwise -> const $ Nothing
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
    iconButton =
      simpleComponent
        { name: "IconButton"
        , propsRow:
            { base: Map.fromFoldable [ children ]
            , generate:
                aria
                  <> [ "classes"
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
            { base:
                Map.fromFoldable
                  [ checkedProp "defaultValue" foreignType
                  , checkedProp "endAdornment" jsx
                  , checkedProp "inputProps" foreignType
                  , checkedProp "inputRef" foreignType
                  , checkedProp "startAdornment" jsx
                  , checkedProp "value" foreignType
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
    inputBase =
      simpleComponent
        { name: "InputBase"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "defaultValue" foreignType
                  , checkedProp "endAdornment" jsx
                  , checkedProp "inputProps" foreignType
                  , checkedProp "inputRef" foreignType
                  , checkedProp "startAdornment" jsx
                  , checkedProp "value" foreignType
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
            { base:
                Map.fromFoldable
                  [ children
                  -- , checkedProp "TypographyClasses" (Type.constructor "MUI.Core.Typography.TypographyClassesKey")
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

    list =
      simpleComponent
        { name: "List"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ children
                  -- component
                  , checkedProp "subheader" jsx
                  ]
            , generate:
                [ "classes"
                , "dense"
                , "disablePadding"
                ]
            }
        , root: rbProps.ul
        }

    -- | TODO: add ContainerComponent and ContainerProps
    listItem =
      simpleComponent
        { name: "ListItem"
        , propsRow:
            { base: Map.fromFoldable [ children ]
            , generate:
                [ "alignItems"
                , "autoFocus"
                , "button"
                , "classes"
                , "dense"
                , "disabled"
                , "disableGutters"
                , "divider"
                , "selected"
                ]
            }
        , root: rbProps.li
        }

    listItemAvatar =
      simpleComponent
        { name: "ListItemAvatar"
        , propsRow:
            { base: Map.fromFoldable [ children ]
            , generate: [ "classes" ]
            }
        , root: rbProps.div
        }

    listItemIcon =
      simpleComponent
        { name: "ListItemIcon"
        , propsRow:
            { base: Map.fromFoldable [ children ]
            , generate: [ "classes" ]
            }
        , root: rbProps.div
        }

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
    listItemText =
      simpleComponent
        { name: "ListItemText"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "primary" jsx
                  , checkedProp "primaryTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyProps")
                  , checkedProp "secondary" jsx
                  , checkedProp "secondaryTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyProps")
                  ]
            , generate:
                [ "classes"
                , "disableTypography"
                , "inset"
                ]
            }
        , root: rbProps.div
        }

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
    menu =
      let
        -- | Still missing: anchorEl, onClose, MenuListProps, PopoverClasses, transitionDuration
        -- | I'm not sure what is the difference between `React.Element` and `DOM.Element`
        handlers =
          map eventHandlerProp
            [ "onClose"
            , "onEnter"
            , "onEntered"
            , "onEntering"
            , "onExit"
            , "onExited"
            , "onExiting"
            ]
      in
        simpleComponent
          { name: "Menu"
          , propsRow:
              { base:
                  Map.fromFoldable
                    $ [ checkedProp "ref" foreignType
                      , children
                      , checkedProp "anchorEl" foreignType
                      , checkedProp "MenuListProps" foreignType
                      , checkedProp "PopoverClasses" foreignType
                      ]
                    <> handlers
              , generate:
                  [ "autoFocus"
                  , "classes"
                  , "disableAutoFocusItem"
                  , "open"
                  , "transitionDuration"
                  , "variant"
                  ]
              }
          -- change to: MUIComponent popover
          , root: rbProps.div
          }

    menuItem =
      simpleComponent
        { name: "MenuItem"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ children
                  , checkedProp "ref" foreignType
                  , forcedProp "component" foreignType false
                  , forcedProp "ListItemClasses" foreignType false
                  ]
            , generate:
                [ "classes"
                , "dense"
                , "disabled"
                , "disableGutters"
                ]
            }
        , root: MUIComponent listItem
        }

    --mobileStepper =
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Paper.PaperPropsRow", Type.constructor "MUI.DOM.Generated.Props_props" ]
    --    , name: "MobileStepper"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ checkedProp "backButton" jsx
    --              , checkedProp "LinearProgressProps" (Type.constructor "MUI.Core.LinearPropgress.LinearProgressOpaqueProps")
    --              , checkedProp "nextButton" jsx
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
    modal =
      let
        handlers =
          map eventHandlerProp
            [ "onBackdropClick"
            , "onClose"
            , "onEscapeKeyDown"
            , "onRendered"
            ]

        -- | How to handle these kind of subprops nicely?
        -- | backdropProps = Type.constructor "MUI.Core.Backdrop.BackdropProps"
        base =
          Map.fromFoldable
            $ [ checkedProp "children" jsx
              , checkedProp "BackdropComponent" foreignType -- (reactComponentApply backdropOpaqueProps)
              , checkedProp "BackdropProps" foreignType -- backdropProps
              -- , container
              -- , checkedProp
              --     "manager"
              --     (Type.constructor "MUI.Core.Modal.ModalManager.ModalManager")
              ]
            <> handlers
      in
        simpleComponent
          { name: "Modal"
          , propsRow:
              { base
              , generate:
                  [ "closeAfterTransition"
                  , "disableAutoFocus"
                  , "disableBackdropClick"
                  , "disableEnforceFocus"
                  , "disableEscapeKeyDown"
                  , "disablePortal"
                  , "disableRestoreFocus"
                  , "disableScrollLock"
                  , "hideBackdrop"
                  , "keepMounted"
                  , "open"
                  ]
              }
          , root: rbProps.div
          }

    ---- | TODO: value
    --nativeSelect=
    --  simpleComponent
    --    { inherits: Just $ MUI.rList
    --        [ Type.constructor "MUI.Core.Input.InputPropsRow", divProps ]
    --    , name: "NativeSelect"
    --    , propsRow:
    --      { base:
    --          Map.fromFoldable
    --              [ children
    --              , checkedProp "IconComponent" jsx
    --              , checkedProp "input" jsx
    --              , checkedProp "inputProps" (Type.constructor "MUI.Core.Input.InputOpaqueProps")
    --              , eventHandlerProp "onChange"
    --              , checkedProp "value" foreignType
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
    --              , checkedProp "fallback" jsx
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
    --              [ checkedProp "defaultValue" foreignType
    --              , checkedProp "endAdornment" jsx
    --              , checkedProp "inputProps" foreignType
    --              , checkedProp "inputRef" foreignType
    --              , checkedProp "startAdornment" jsx
    --              , checkedProp "value" foreignType
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
    -- grid =
    --   { extraDeclarations: []
    --   , modulePath:
    --       { input: Name "Grid"
    --       , output: Name "Grid"
    --       }
    --   , propsRow:
    --       { base: Map.fromFoldable [ children ]
    --       , generate:
    --           [ "alignContent"
    --           , "alignItems"
    --           , "classes"
    --           , "container"
    --           , "direction"
    --           , "item"
    --           , "justify"
    --           , "lg"
    --           , "md"
    --           , "sm"
    --           , "spacing"
    --           , "wrap"
    --           , "xl"
    --           , "xs"
    --           , "zeroMinWidth"
    --           ]
    --       , ts:
    --           { instantiation: Nothing
    --           , unionName:
    --               \property members -> case property of
    --                 _
    --                   | property `Array.elem` [ "xs", "sm", "md", "lg", "xl" ] -> Just $ TypeName "GridSize"
    --                 "spacing" -> Just $ TypeName "GridSpacing"
    --                 "justify" -> Just $ TypeName "GridJustification"
    --                 otherwise -> Nothing
    --           }
    --       }
    --   , root: rbProps.div
    --   }
    pagination =
      { extraDeclarations: []
      , modulePath:
          { input: Path "lab" $ Name "Pagination"
          , output: Path "Lab" $ Name "Pagination"
          }
      , propsRow:
          { base:
              Map.fromFoldable
                $ [ checkedProp "count" int
                  , eventHandlerProp "onChange"
                  , forcedProp "boundryCount" int false
                  , checkedProp "siblingCount" int
                  ]
          , generate:
              [ "classes"
              , "color"
              , "defaultPage"
              , "disabled"
              -- | TODO: move to base
              -- | function(type: string, page: number, selected: bool) => string
              -- | , "getItemAraiaLabel"
              , "hideNextButton"
              , "hidePrevButton"
              , "page"
              , "shape"
              , "showFirstButton"
              , "showLastButton"
              , "size"
              , "variant"
              ]
          , ts: { instantiation: Nothing, unionName: \_ _ -> Nothing }
          }
      , root: rbProps.div
      }

    paper =
      simpleComponent
        { name: "Paper"
        , propsRow:
            { base: Map.fromFoldable $ [ children ]
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
    --              [ checkedProp "action" foreignType
    --              , checkedProp "anchorEl" foreignType
    --              , checkedProp "anchorPosition" foreignType
    --              , children
    --              , checkedProp "getContentAnchorEl" foreignType
    --              , eventHandlerProp "onChange"
    --              , eventHandlerProp "onEnter"
    --              , eventHandlerProp "onEntering"
    --              , eventHandlerProp "onExit"
    --              , eventHandlerProp "onExited"
    --              , eventHandlerProp "onExiting"
    --              , checkedProp "PaperProps" (Type.constructor "MUI.Core.Paper.PaperOpaqueProps")
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
    --              [ checkedProp "anchorEl" foreignType
    --              , children
    --              , checkedProp "modifiers" foreignType
    --              , checkedProp "popperOptions" foreignType
    --              , checkedProp "popperRef" foreignType
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
    --              , checkedProp "container" foreignType
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
    --              [ checkedProp "checkedIcon" jsx
    --              , checkedProp "icon" jsx
    --              , checkedProp "inputProps" foreignType
    --              , checkedProp "inputRef" foreignType
    --              , eventHandlerProp "onChange"
    --              , checkedProp "value" foreignType
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
    --              , checkedProp "defaultValue" foreignType
    --              , eventHandlerProp "onChange"
    --              , checkedProp "value" foreignType
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
    --              [ checkedProp "rootRef" foreignType
    --              ]
    --      , generate:
    --        []
    --      }
    --    }
    ---- | TODO: value
    select =
      simpleComponent
        { name: "Select"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  , checkedProp "defaultValue" foreignType
                  , checkedProp "IconComponent" jsx
                  , checkedProp "input" jsx
                  , checkedProp "inputProps" foreignType
                  , checkedProp "label" jsx
                  , checkedProp "MenuProps" foreignType
                  , eventHandlerProp "onChange"
                  , eventHandlerProp "onClose"
                  , eventHandlerProp "onOpen"
                  , checkedProp "renderValue" foreignType
                  , checkedProp "SelectDisplayProps" foreignType
                  , checkedProp "value" string
                  ]
            , generate:
                [ "autoWidth"
                , "classes"
                , "displayEmpty"
                , "id"
                , "labelId"
                , "labelWidth"
                , "multiple"
                , "native"
                , "open"
                , "renderValue"
                , "variant"
                ]
            }
        , root: MUIComponent input
        }

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
    --              , checkedProp "defaultValue" foreignType
    --              , checkedProp "getAriaLabel" foreignType
    --              , checkedProp "getAriaValueText" foreignType
    --              , eventHandlerProp "onChange"
    --              , eventHandlerProp "onChangeCommitted"
    --              , checkedProp "marks" foreignType
    --              , checkedProp "value" foreignType
    --              , checkedProp "valueLabelFormat" foreignType
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
    snackbar =
      simpleComponent
        { name: "Snackbar"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  , checkedProp "action" jsx
                  , checkedProp "ClickAwayListenerProps" foreignType
                  , checkedProp "ContentProps" foreignType
                  -- `key` is in the docs but not in the typedef
                  , checkedProp "key" foreignType
                  , checkedProp "message" jsx
                  , eventHandlerProp "onClose"
                  , eventHandlerProp "onEnter"
                  , eventHandlerProp "onEntered"
                  , eventHandlerProp "onEntering"
                  , eventHandlerProp "onExit"
                  , eventHandlerProp "onExited"
                  , eventHandlerProp "onExiting"
                  , checkedProp "TransitionComponent" foreignType
                  , checkedProp "TransitionProps" foreignType
                  ]
            , generate:
                [ "anchorOrigin"
                , "autoHideDuration"
                , "classes"
                , "disableWindowBlurListener"
                , "open"
                , "resumeHideDuration"
                , "transitionDuration"
                ]
            }
        , root: rbProps.div
        }

    snackbarContent =
      simpleComponent
        { name: "SnackbarContent"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , checkedProp "action" jsx
                  , checkedProp "message" jsx
                  ]
            , generate:
                [ "classes"
                , "role"
                ]
            }
        , root: MUIComponent paper
        }

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
    --              , checkedProp "icon" foreignType
    --              , checkedProp "optional" jsx
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
    --                [ checkedProp "icon" jsx
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
    --              , checkedProp "icon" jsx
    --              , checkedProp "optional" jsx
    --              , checkedProp "StepIconComponent" foreignType
    --              , checkedProp "StepIconProps" (Type.constructor "MUI.Core.StepIcon.StepIconOpaqueProps")
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
    --              , checkedProp "connector" jsx
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
    svgIcon =
      simpleComponent
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
    --              , checkedProp "SwipeAreaProps" foreignType
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
    --              [ checkedProp "checkedIcon" jsx
    --              , checkedProp "icon" jsx
    --              , checkedProp "inputProps" foreignType
    --              , checkedProp "inputRef" foreignType
    --              , eventHandlerProp "onChange"
    --              , checkedProp "value" foreignType
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
    --              , checkedProp "icon" jsx
    --              , checkedProp "label" jsx
    --              , checkedProp "value" foreignType
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
    table =
      simpleComponent
        { name: "Table"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  ]
            , generate:
                [ "classes"
                , "padding"
                , "size"
                , "stickyHeader"
                ]
            }
        , root: rbProps.div
        }

    tableBody =
      simpleComponent
        { name: "TableBody"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  ]
            , generate:
                [ "classes"
                ]
            }
        , root: rbProps.div
        }

    tableCell =
      simpleComponent
        { name: "TableCell"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  ]
            , generate:
                [ "classes"
                , "padding"
                , "scope"
                , "size"
                , "sortDirection"
                , "variant"
                ]
            }
        , root: rbProps.div
        }

    tableContainer =
      simpleComponent
        { name: "TableContainer"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  ]
            , generate:
                [ "classes"
                ]
            }
        , root: rbProps.div
        }

    tableFooter =
      simpleComponent
        { name: "TableFooter"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  ]
            , generate:
                [ "classes"
                ]
            }
        , root: rbProps.div
        }

    tableHead =
      simpleComponent
        { name: "TableHead"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  ]
            , generate:
                [ "classes"
                ]
            }
        , root: rbProps.div
        }

    tableRow =
      simpleComponent
        { name: "TableRow"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , children
                  ]
            , generate:
                [ "classes"
                , "hover"
                , "selected"
                ]
            }
        , root: rbProps.div
        }

    tableSortLabel =
      simpleComponent
        { name: "TableSortLabel"
        , propsRow:
            { base:
                Map.fromFoldable
                  [ checkedProp "ref" foreignType
                  , checkedProp "IconComponent" foreignType
                  , children
                  ]
            , generate:
                [ "active"
                , "classes"
                , "direction"
                , "hideSortIcon"
                ]
            }
        , root: MUIComponent buttonBase
        }

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
    --            , checkedProp "backIconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonOpaqueProps")
    --            , checkedProp "labelDisplayedRows" foreignType
    --            , checkedProp "labelRowsPerPage" jsx
    --            , checkedProp "nextIconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonOpaqueProps")
    --            , eventHandlerProp "onChangePage"
    --            , eventHandlerProp "onChangeRowsPerPage"
    --            , checkedProp "SelectProps" (Type.constructor "MUI.Core.Select.SelectOpaqueProps")
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
    --tabs =
    --  simpleComponent
    --    { inherits: Nothing
    --    , name: "Tabs"
    --    , propsRow:
    --      { base: Map.fromFoldable
    --        [ children
    --        , checkedProp "action" foreignType
    --        , eventHandlerProp "onChange"
    --        , checkedProp "ScrollButtonComponent" foreignType
    --        , checkedProp "TabIndicatorProps" foreignType
    --        , checkedProp "value" foreignType
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
          { input: Path "core" $ Name "TextField"
          , output: Path "Core" $ Path "TextField" (Name textFieldType)
          }
      , propsRow:
          { base:
              Map.fromFoldable
                [ children
                , checkedProp "defaultValue" foreignType
                , checkedProp "helperText" jsx
                --, checkedProp "InputLabelProps" (Type.constructor "MUI.Core.InputLabel.InputLabelOpaqueProps")
                -- , checkedProp "inputProps" foreignType
                -- , checkedProp "inputRef" foreignType
                --, checkedProp "FormHelperTextProps" (Type.constructor "MUI.Core.FormHelperText.FormHelperTextOpaqueProps")
                , checkedProp "label" jsx
                , eventHandlerProp "onChange"
                , eventHandlerProp "onBlur"
                , eventHandlerProp "onFocus"
                --, checkedProp "SelectProps" (Type.constructor "MUI.Core.Select.SelectOpaqueProps")
                , checkedProp "value" string
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
              { instantiation:
                  Just
                    { strategy: TypeAlias
                    , extractProps:
                        \defaultInstance -> case unroll defaultInstance of
                          ( Instantiation.Union
                              [ Mu.In (Instantiation.Object fqn1 props1)
                            , Mu.In (Instantiation.Object fqn2 props2)
                            , Mu.In (Instantiation.Object fqn3 props3)
                            ]
                          ) -> case unionMember of
                            1 -> pure { fqn: fqn1, props: props1 }
                            2 -> pure { fqn: fqn2, props: props2 }
                            otherwise -> pure { fqn: fqn3, props: props3 }
                          --  throwError
                          --    [ "Not sure how to tackle this three props types: " <> fqn1 <> ", " <> fqn2 <> "," <> fqn3 ]
                          (Instantiation.Union _) ->
                            throwError
                              [ "Expecting a two member union as a representation for TextField" ]
                          i ->
                            throwError
                              [ "Expecting an union as a representation for TextField got: " <> unsafeStringify i ]
                    }
              , unionName: \_ _ -> Nothing
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
    --              , checkedProp "value" foreignType
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
            { base:
                Map.fromFoldable
                  [ children
                  , forcedProp "component" (roll TypeString) false
                  ]
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
    , avatar
    , backdrop
    , badge
    -- , bottomNavigation
    -- , bottomNavigationAction
    , box
    , breadcrumbs
    , buttonBase
    , buttonGroup
    , button
    , card
    -- , cardActionArea
    , cardActions
    -- , cardContent
    , cardHeader
    -- , cardMedia
    , circularProgress
    -- , clickAwayListener
    , checkbox
    -- , chip
    , collapse
    , container
    -- , cssBaseline
    , dialog
    , dialogActions
    , dialogContent
    , dialogContentText
    , dialogTitle
    , divider
    , drawer
    -- , expansionPanel
    -- , expansionPanelActions
    -- , expansionPanelDetails
    -- , expansionPanelSummary
    , fab
    , fade
    , formControl
    , formControlLabel
    , formGroup
    , formHelperText
    , formLabel
    , grid
    , gridList
    , gridListTile
    -- , gridListTileBar
    , grow
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
    , list
    , listItem
    , listItemAvatar
    , listItemIcon
    -- , listItemSecondaryAction
    , listItemText
    -- , listSubheader
    , menu
    , menuItem
    , modal
    -- , nativeSelect
    -- , noSsr
    , pagination
    , paper
    -- , popover
    -- , popper
    -- , portal
    -- , radio
    -- , radioGroup
    -- , rootRef
    , select
    -- , slide
    -- , slider
    , snackbar
    , snackbarContent
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
    , table
    , tableBody
    , tableCell
    , tableFooter
    , tableHead
    -- , tablePagination
    , tableRow
    , tableSortLabel
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
multiString :: forall a. Pattern -> ReadM a -> ReadM (Array a)
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

commaSeparatedComponentList :: { helpText :: String, long :: String, short :: Char } -> Parser (Array Component)
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
    importAliases =
      Map.fromFoldable
        $ [ ModuleName "Unsafe.Coerce" /\ Nothing
          , ModuleName "Unsafe.Reference" /\ Nothing
          , ModuleName "MUI.Core" /\ Nothing
          , ModuleName "React.Basic" /\ Nothing
          , ModuleName "MUI.DOM.Generated" /\ Just "DOM"
          , ModuleName "MUI.System.Display" /\ Nothing
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
        >>= \c -> do
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
