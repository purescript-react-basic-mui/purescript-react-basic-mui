module Main where

import Prelude

import Codegen (Codegen(..), componentJSFile, componentPSFile, iconJSFile, iconPSFile, icons)
import Codegen (component, icon, write) as Codegen
import Codegen.AST (Ident(..), ModuleName(..), TypeF(..), TypeName(..))
import Codegen.AST.Sugar (declType)
import Codegen.AST.Sugar.Type (app, constructor, record, row) as Type
import Codegen.Model (Component, Icon, ModulePath(..), arrayJSX, componentFullPath, divProps, iconName, jsx, psImportPath, reactComponentApply)
import Codegen.Model (componentName) as Model
import Codegen.TS.MUI (componentProps) as TS.MUI
import Codegen.TS.MUI (propsTypeName)
import Control.Alt ((<|>))
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter)
import Data.Array (null, sort) as Array
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
    eventHandlerProp name = Tuple name (Type.constructor "React.Basic.Events.EventHandler")

    foreignType = Type.constructor "Foreign.Foreign"
    jss = Type.constructor "MUI.Core.JSS"
    -- | variable used For example:
    -- | type AppBarPropsOptions componentProps = (... | componentProps)
    componentPropsIdent = Ident "componentProps"

    component = Tuple "component" $ reactComponentApply
        (Type.record <<< Type.row mempty $ Just $ Left componentPropsIdent)

    defaultComponent = Tuple "defaultComponent" $ reactComponentApply
        (Type.record <<< Type.row mempty $ Just $ Left componentPropsIdent)

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
          (Type.constructor "MUI.Core.Paper.PaperPropsOptions")
          [ divProps ]
      , name: "AppBar"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children ]
        , generate: ["classes", "color", "position"]
        }
      }
    avatar = simpleComponent
      { inherits: Just $ divProps
      , name: "Avatar"
      , propsType: 
          { base: basePropsRow [] $ Map.fromFoldable []
          , generate: [ 
              "alt"
            , "classes"
            -- not sure what to do here, "imgProps"
            , "sizes"
            , "src"
            , "srcSet"
            , "variant"
            ]
          }
      }

    backdrop = simpleComponent
      { inherits: Just $ Type.app
          (Type.constructor "MUI.Core.Fade.FadePropsOptions")
          [ divProps ]
      , name: "Backdrop"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children ]
        , generate: [ "classes", "invisible", "open", "transitionDuration" ]
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

    bottomNavigation = simpleComponent
      { inherits: Just $ divProps
      , name: "BottomNavigation"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable $ [ children, component ] <> (map eventHandlerProp ["onChange"])
          , generate: ["classes", "showLabels"]
          }
      }

    box = simpleComponent
      { inherits: Just $ divProps
      , name: "Box"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable $ [ children, component, Tuple "css" jss ]
        , generate: [ "clone" ]
        }
      }

    breadcrumbs = simpleComponent
      { inherits: Just $ divProps
      , name: "Breadcrumbs"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable $ 
            [ children
            -- Not found on the TS side
            -- , component
            , Tuple "separator" jsx
            , Tuple "ref" foreignType
            ]
        , generate: [ "classes", "itemsAfterCollapse", "itemsBeforeCollapse", "maxItems" ]
        }
      }

    button = simpleComponent
      { inherits: Just $ Type.app
        (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions")
        [Type.constructor "React.Basic.DOM.Props_button"]
      , name: "Button"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable
          [ Tuple "endIcon" jsx
          , Tuple "startIcon" jsx
          ]
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
          -- XXX: We are catching material ui documentation / type error here.
          -- ButtonBase doesn't contain `component` prop.
          -- , component
          , eventHandlerProp "onFocusVisible"
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
            [ "centerRipple", "classes", "color", "disabled"
            , "disableRipple", "focusRipple", "focusVisibleClassName"
            , "type"
            ]
          }
        , tsc: { strictNullChecks: false }
        }

    card = simpleComponent
      { inherits: Just $ Type.app
          (Type.constructor "MUI.Core.Paper.PaperPropsOptions")
          [ divProps ]
      , name: "Card"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children ]
        , generate: [ "classes", "raised" ]
        }
      }

    cardActionArea = simpleComponent
      { inherits: Just $ Type.app
          (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions")
          [Type.constructor "React.Basic.DOM.Props_button"]
      , name: "CardActionArea"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children ]
        , generate: [ "classes" ]
        }
      }

    cardActions = simpleComponent
      { inherits: Just $ divProps
      , name: "CardActions"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children ]
        , generate: [ "classes", "disableSpacing" ]
        }
      }

    cardContent = simpleComponent
      { inherits: Just $ divProps
      , name: "CardContent"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children, component ]
        , generate: [ "classes" ]
        }
      }

    cardHeader = simpleComponent
      { inherits: Just $ divProps
      , name: "CardHeader"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
            [ Tuple "action" jsx
            , Tuple "avatar" jsx
            , children
            , component
            , Tuple "subheader" jsx
            , Tuple "subheaderTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyProps")
            , Tuple "title" jsx
            , Tuple "titleTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyProps")
            ]
        , generate: [ "classes", "disableTypography" ]
        }
      }

    cardMedia = simpleComponent
      { inherits: Just $ divProps
      , name: "CardMedia"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
            [ children
            -- This isn't being found in the .d.ts
            --, component 
            ]
        , generate: [ "classes", "image", "src" ]
        }
      }

    checkbox = simpleComponent
      { inherits: Nothing -- should be IconButon
      , name: "Checkbox"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
            ([Tuple "checkedIcon" jsx
            , Tuple "icon" jsx
            , Tuple "indeterminateIcon" jsx
            , Tuple "inputProps" foreignType
            , Tuple "inputRef" foreignType
            , Tuple "value" foreignType
            ] <> (map eventHandlerProp [ "onChange" ]))
        , generate: 
            [ "checked"
            , "classes"
            , "color"
            , "disabled"
            , "disableRipple"
            , "id"
            , "indeterminate"
            , "required"
            , "type"
            ]
        }
      }

    chip = simpleComponent
      { inherits: Just $ divProps
      , name: "Chip"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
            ([Tuple "avatar" jsx
            , Tuple "deleteIcon" jsx
            , Tuple "icon" jsx
            , Tuple "label" jsx
            ] <> (map eventHandlerProp [ "onDelete" ]))
        , generate: 
            [ "classes"
            , "color"
            , "disabled"
            , "size"
            , "variant"
            ]
        }
      }

    circularProgress = simpleComponent
      { inherits: Just $ divProps
      , name: "CircularProgress"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable []
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
      }

    clickAwayListener =
      let
        onClickAway = eventHandlerProp "onClickAway"
        -- | Single jsx node is required
        child = Tuple "children" jsx
        base = basePropsRow [] $ Map.fromFoldable [ child, onClickAway ]
      in simpleComponent
        { inherits: Nothing
        , name: "ClickAwayListener"
        , propsType:
          { base
          , generate: [ "mouseEvent", "touchEvent" ]
          }
        }

    collapse = simpleComponent
      { inherits: Nothing -- should extend Transition
      , name: "Collapse"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children, component ]
        , generate: 
            [ "collapsedHeight" 
            , "timeout"
            ]
        }
      }

    container = simpleComponent
      { inherits: Just $ divProps
      , name: "Container"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ component ]
        , generate: 
            [ "fixed" 
            , "maxWidth"
            ]
        }
      }

    cssBaseline = simpleComponent
      { inherits: Nothing
      , name: "CssBaseline"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children ]
        , generate: []
        }
      }

    dialog =
      let
        -- | TODO:
        -- | migration.
        -- | * `PaperComponent`, `PaperProps`, `TransitionComponent`, `TransitionDuration`
        handlers = map eventHandlerProp
          [ "onEnter", "onEntered", "onEntering"
          , "onExit", "onExited", "onExiting"
          ]
        base = basePropsRow [] $ Map.fromFoldable $ [ children ] <> handlers
      in simpleComponent
        { inherits: Just $ Type.app
            (Type.constructor "MUI.Core.Modal.ModalPropsOptions")
            [ divProps ]
        , name: "Dialog"
        , propsType:
          { base
          , generate:
            [ "aria-describedby", "aria-labelledby"
            , "classes", "fullScreen", "fullWidth"
            , "maxWidth", "scroll", "transitionDuration"
            ]
          }
        }

    dialogActions = simpleComponent
      { inherits: Nothing
      , name: "DialogActions"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children ]
        , generate: [ "classes", "disableSpacing" ]
        }
      }

    dialogContent = simpleComponent
      { inherits: Nothing
      , name: "DialogContent"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children ]
        , generate: [ "classes", "dividers" ]
        }
      }

    dialogTitle = simpleComponent
      { inherits: Nothing
      , name: "DialogTitle"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable [ children ]
        , generate: [ "classes", "disableTypography" ]
        }
      }

    -- | TODO: add component
    divider = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_hr"
      , name: "Divider"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable []
        , generate: 
            [ "absolute"
            , "classes"
            , "light"
            , "orientation"
            , "variant"
            ]
        }
      }

    drawer = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.Modal.ModalPropsOptions") [ divProps ]
      , name: "Drawer"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              ([ children
              , Tuple "ModalProps" (Type.constructor "MUI.Core.Modal.ModalPropsPartial")
              , eventHandlerProp "onClose"
              , Tuple "PaperProps" (Type.constructor "MUI.Core.Modal.ModalPropsPartial")
              , Tuple "SlideProps" (Type.constructor "MUI.Core.Slide.SlidePropsPartial")
              ] <> map eventHandlerProp ["onClose", "onEnter", "onEntered", "onEntering", "onExit", "onExited", "onExiting"])
          , generate: 
            [ "anchor"
            , "classes"
            , "elevation"
            , "open"
            , "transitionDuration"
            , "variant"
            ]
          }
      }

    -- | TODO: TransitionComponent, TransitionProps
    expansionPanel = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.Paper.PaperPropsOptions") [ divProps ]
      , name: "ExpansionPanel"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              , eventHandlerProp "onChange"
              ]
          , generate: 
              [ "classes"
              , "defaultExpanded"
              , "disabled"
              , "expanded"
              ]
          }
      }

    expansionPanelActions = simpleComponent
      { inherits: Just divProps
      , name: "ExpansionPanelActions"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              ]
          , generate: 
              [ "classes"
              ]
          }
      }

    expansionPanelDetails = simpleComponent
      { inherits: Just divProps
      , name: "ExpansionPanelDetails"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              ]
          , generate: 
              [ "classes"
              ]
          }
      }

    expansionPanelSummary = simpleComponent
      { inherits: Just divProps
      , name: "ExpansionPanelSummary"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              , Tuple "expandIcon" jsx
              , Tuple "IconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonPropsPartial")
              ]
          , generate: 
              [ "classes"
              ]
          }
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

    -- | TODO: TransitionComponent
    fade = simpleComponent
      { inherits: Nothing -- should inherit TransitionComponent
      , name: "Fade"
      , propsType: 
          { base: basePropsRow [] $ Map.fromFoldable [ Tuple "ref" foreignType ]
          , generate: [ 
            -- not sure what to do here, "theme"
            ]
          }
      }

    -- | TODO inputComponent, make value a type variable
    filledInput = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.InputBasePropsOption") [ divProps ]
      , name: "FilledInput"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              , Tuple "endAdornment" jsx
              , Tuple "inputProps" (Type.constructor "MUI.Core.InputBasePartial")
              , Tuple "inputRef" foreignType
              , eventHandlerProp "onChange"
              , Tuple "startAdornment" jsx
              , Tuple "value" foreignType
              ]
          , generate: 
              [ "autoComplete"
              , "autoFocus"
              , "classes"
              , "className"
              , "color"
              , "defaultValue"
              , "disabled"
              , "disableUnderline"
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
      }

    formControl = simpleComponent
      { inherits: Just $ divProps
      , name: "FormControl"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              ]
          , generate: 
              [ "color"
              , "disabled"
              , "error"
              , "fullWidth"
              , "hiddenLabel"
              , "margin"
              , "required"
              , "variant"
              ]
          }
      }

    -- | TODO make value a type variable
    formControlLabel = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_label" 
      , name: "FormControlLabel"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              , Tuple "control" jsx
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
      }

    formGroup = simpleComponent
      { inherits: Just $ divProps
      , name: "FormGroup"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              ]
          , generate: 
              [ "classes"
              , "row"
              ]
          }
      }

    
    formHelperText = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_p" 
      , name: "FormHelperText"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              , component
              ]
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
      }

    formLabel = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_label" 
      , name: "FormLabel"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              ]
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
      }

    grid = simpleComponent
      { inherits: Just divProps
      , name: "Grid"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              ]
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
          }
      }


    gridList =
      let
        base = basePropsRow [] $ Map.fromFoldable
          [ children
          , component
          ]
        in simpleComponent
          { inherits: Just $ Type.constructor "React.Basic.DOM.Props_ul"
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
          { inherits: Just $ Type.constructor "React.Basic.DOM.Props_li"
          , name: "GridListTile"
          , propsType:
            { base
            , generate: [ "classes", "cols", "rows" ]
            }
          }

    gridListTileBar = simpleComponent
      { inherits: Nothing
      , name: "GridListTileBar"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ Tuple "actionIcon" jsx
              , Tuple "subtitle" jsx
              , Tuple "title" jsx
              ]
          , generate: 
              [ "classes"
              , "actionPosition"
              , "titlePosition"
              ]
          }
      }



    -- | TODO update when Transition is figured out
    grow = simpleComponent
      { inherits: Nothing
      , name: "Grow"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ 
              ]
          , generate: 
              [ "in"
              , "timeout"
              ]
          }
      }

    hidden = simpleComponent
      { inherits: Nothing
      , name: "Hidden"
      , propsType:
          { base: emptyBase 
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
          }
      }


    icon = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_span"
      , name: "Icon"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ component
              ]
          , generate: 
              [ "classes" 
              , "color"
              , "fontSize"
              ]
          }
      }

    iconButton = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions") [ divProps ]
      , name: "IconButton"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable 
              [ children
              ]
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
      }

    input = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.InputBase.InputBasePropsOptions") [ divProps ]
      , name: "Input"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
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
      }


    inputAdornment = simpleComponent
      { inherits: Just divProps
      , name: "InputAdornment"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
            [ children
            , component
            ]
        , generate:
          [ "classes" 
          , "disablePointerEvents"
          , "disableTypography"
          , "position"
          , "variant"
          ]
        }
      }



    -- | TODO: inputProps should be something like ReactComponent { | InputProps }
    inputBase = simpleComponent
      { inherits: Just $ divProps
      , name: "InputBase"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
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
      }

    inputLabel = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.FormLabel.FormLabelPropsOptions") [ Type.constructor "React.Basic.DOM.Props_label" ]
      , name: "InputLabel"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
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
      }

    linearProgress = simpleComponent
      { inherits: Just divProps
      , name: "LinearProgress"
      , propsType:
        { base: emptyBase
        , generate:
          [ "classes", "color", "value"
          , "valueBuffer", "variant"
          ]
        }
      }

    link = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_a"
      , name: "Link"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
          [ children
          , Tuple "TypographyClasses" (Type.constructor "MUI.Core.Typography.TypographyClassKey")
          ] 
        , generate:
          [ "classes" 
          , "color"
          , "underline"
          , "variant"
          ]
        }
      }

    list = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_ul"
      , name: "List"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
          [ children
          , Tuple "subheader" jsx
          ] 
        , generate:
          [ "classes" 
          , "dense"
          , "disablePadding"
          ]
        }
      }

    -- | TODO add ContainerComponent and ContainerProps
    listItem = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_li"
      , name: "ListItem"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
          [ children
          ] 
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
      }

    listItemAvatar = simpleComponent
      { inherits: Nothing
      , name: "ListItemAvatar"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
          [
          ] 
        , generate:
          [ "classes"
          ]
        }
      }

    listItemIcon = simpleComponent
      { inherits: Just divProps
      , name: "ListItemIcon"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
          [ 
          ] 
        , generate:
          [ "classes"
          ]
        }
      }

    listItemSecondaryAction = simpleComponent
      { inherits: Just divProps
      , name: "ListItemSecondaryAction"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
          [
          ] 
        , generate:
          [ "classes"
          ]
        }
      }

    listItemText = simpleComponent
      { inherits: Just divProps
      , name: "ListItemText"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
          [ Tuple "primary" jsx
          , Tuple "primaryTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyClassKey")
          , Tuple "secondary" jsx
          , Tuple "secondaryTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyClassKey")
          ] 
        , generate:
          [ "classes"
          , "disableTypography"
          , "inset"
          ]
        }
      }

    listSubheader = simpleComponent
      { inherits: Just $ (Type.constructor "React.Basic.DOM.Props_li")
      , name: "ListSubheader"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
          [ children 
          ] 
        , generate:
          [ "classes"
          , "color"
          , "disableGutters"
          , "disableSticky"
          , "inset"
          ]
        }
      }

    menu =
      let
        -- | Still missing: anchorEl, onClose, MenuListProps, PopoverClasses, transitionDuration
        handlers = map eventHandlerProp
          ["onClose", "onEnter", "onEntered", "onEntering", "onExit", "onExited", "onExiting"]
        -- | I'm not sure what is the difference between `React.Element` and `DOM.Element`
        nullable = Type.constructor "Data.Nullable.Nullable"
        domElement = Type.constructor "Web.DOM.Element"
        anchorEl = Tuple "anchorEl" $ Type.app nullable [ domElement ]
        base = basePropsRow [] $ Map.fromFoldable $ [ anchorEl, children ] <> handlers
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
          -- XXX: We are catching material ui documentation / type error here.
          -- MenuItem doesn't contain `component` prop.
          -- , component
          ]
      in simpleComponent
        { inherits: Just $ Type.app (Type.constructor "MUI.Core.ListItem.ListItemPropsOptions") [ Type.constructor "React.Basic.DOM.Props_li" ]
        , name: "MenuItem"
        , propsType:
          { base
          , generate: [ "classes", "dense", "disableGutters" ]
          }
        }

    mobileStepper = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.Paper.PaperPropsOptions") [ Type.constructor "React.Basic.DOM.Props_props" ]
        , name: "MobileStepper"
        , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ Tuple "backButton" jsx
              , Tuple "LinearProgressProps" (Type.constructor "MUI.Core.LinearPropgress.LinearProgressProps")
              , Tuple "nextButton" jsx
              ]
          , generate: 
              [ "activeStep"
              , "backButton"
              , "classes"
              , "position"
              , "steps"
              , "variant"
              ]
          }
        }

    modal =
      let
        handlers = map eventHandlerProp
          [ "onBackdropClick"
          , "onClose"
          , "onEscapeKeyDown"
          , "onRendered"
          ]

        backdropPropsPartial = Type.constructor "MUI.Core.Backdrop.BackdropPropsPartial"
        base = basePropsRow [ ] $ Map.fromFoldable $
          [ children
          , Tuple "BackdropComponent" (reactComponentApply backdropPropsPartial)
          , Tuple "BackdropProps" backdropPropsPartial
          -- , container
          , Tuple
              "manager"
              (Type.constructor "MUI.Core.Modal.ModalManager.ModalManager")
          ]
          <> handlers
      in simpleComponent
        { inherits: Nothing
        , name: "Modal"
        , propsType:
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
        }

    -- | TODO value
    nativeSelect = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.Input.InputPropsOptions") [ divProps ]
        , name: "NativeSelect"
        , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "IconComponent" jsx
              , Tuple "input" jsx
              , Tuple "inputProps" (Type.constructor "MUI.Core.Input.InputProps")
              , eventHandlerProp "onChange"
              , Tuple "value" foreignType 
              ]
          , generate: 
              [ "classes"
              , "variant"
              ]
          }
        }

    -- | TODO value
    noSsr = simpleComponent
      { inherits: Nothing
        , name: "NoSsr"
        , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "fallback" jsx
              ]
          , generate: 
              [ "defer"
              ]
          }
        }

    -- | inputComponent
    outlineInput = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.InputBase.InputBasePropsOptions") [ divProps ]
      , name: "OutlinedInput"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
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
          , "notched"
          , "placeholder"
          , "readOnly"
          , "required"
          , "rows"
          , "rowsMax"
          , "type"
          ]
        }
      }



    paper = simpleComponent
      { inherits: Just divProps
      , name: "Paper"
      , propsType:
        { base: basePropsRow [ ] $ Map.fromFoldable $ [ children, component ]
        , generate:
          [ "classes"
          , "elevation"
          , "square"
          ]
        }
      }

    -- | TransitionComponent, TransitionProps, transformOrigin
    popover = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.Modal.ModalPropsOptions") [ divProps ]
      , name: "Popover"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
            [ Tuple "action" foreignType
            , Tuple "anchorEl" foreignType
            , Tuple "anchorPosition" foreignType
            , children
            , Tuple "getContentAnchorEl" foreignType
            , eventHandlerProp "onChange"
            , eventHandlerProp "onEnter"
            , eventHandlerProp "onEntering"
            , eventHandlerProp "onExit"
            , eventHandlerProp "onExited"
            , eventHandlerProp "onExiting"
            , Tuple "PaperProps" (Type.constructor "MUI.Core.Paper.PaperProps")
            ]
        , generate:
          [ "anchorOrigin"
          , "anchorPosition"
          , "classes"
          , "elevation"
          , "marginThreshold"
          , "open"
          --, "transformOrigin"
          , "transitionDuration"
          ]
        }
      }

    popper = simpleComponent
      { inherits: Just divProps
      , name: "Popper"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
            [ Tuple "anchorEl" foreignType
            , children
            , Tuple "modifiers" foreignType
            , Tuple "popperOptions" foreignType
            , Tuple "popperRef" foreignType
            ]
        , generate:
          [ "disablePortal"
          , "keepMounted"
          , "open"
          , "transition"
          ]
        }
      }

    portal = simpleComponent
      { inherits: Nothing
      , name: "Portal"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
          [ children
          , Tuple "container" foreignType
          , eventHandlerProp "onRendered"
          ]
        , generate: [ "disablePortal" ]
        }
      }

    -- | value, inputProps, inputRef
    radio = simpleComponent
        { inherits: Just $ Type.app (Type.constructor "MUI.Core.IconButton.IconButtonPropsOptions") [ Type.constructor "React.Basic.DOM.Props_button" ]
        , name: "Radio"
        , propsType: 
            { base: basePropsRow [] $ Map.fromFoldable
                  [ Tuple "checkedIcon" jsx
                  , Tuple "icon" jsx
                  , Tuple "inputProps" foreignType
                  , Tuple "inputRef" foreignType
                  , eventHandlerProp "onChange"
                  , Tuple "value" foreignType
                  ]
            , generate: 
              [ "checked"
              , "classes"
              , "color"
              , "disabled"
              , "disableRipple"
              , "id"
              , "name"
              , "required"
              , "type"
              ]
            }
        }

    -- | value, inputProps, inputRef
    radioGroup = simpleComponent
        { inherits: Just $ Type.app (Type.constructor "MUI.Core.FormGroup.FormGroupPropsOptions") [ divProps ]
        , name: "RadioGroup"
        , propsType: 
            { base: basePropsRow [] $ Map.fromFoldable
                  [ children
                  , Tuple "defaultValue" foreignType
                  , eventHandlerProp "onChange"
                  , Tuple "value" foreignType
                  ]
            , generate: 
              [ "name"
              ]
            }
        }

    rootRef = simpleComponent
        { inherits: Nothing
        , name: "RootRef"
        , propsType: 
            { base: basePropsRow [] $ Map.fromFoldable
                  [ Tuple "rootRef" foreignType
                  ]
            , generate: 
              [
              ]
            }
        }

    -- | TODO value
    select = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.Input.InputPropsOptions") [ divProps ]
        , name: "Select"
        , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "IconComponent" jsx
              , Tuple "input" jsx
              , Tuple "inputProps" (Type.constructor "MUI.Core.Input.InputProps")
              , Tuple "MenuProps" (Type.constructor "MUI.Core.Menu.MenuProps")
              , eventHandlerProp "onChange"
              , eventHandlerProp "onClose"
              , eventHandlerProp "onOpen"
              , Tuple "renderValue" foreignType
              , Tuple "SelectDisplayProps" foreignType
              , Tuple "value" foreignType 
              ]
          , generate: 
              [ "autoWidth"
              , "classes"
              , "displayEmpty"
              , "labelWidth"
              , "multiple"
              , "native"
              , "open"
              , "renderValue"
              , "variant"
              ]
          }
        }

    slide = simpleComponent
      { inherits: Nothing
      , name: "Slide"
      , propsType: 
        { base: basePropsRow [] $ Map.fromFoldable $ 
            map eventHandlerProp ["onEnter", "onEntered", "onEntering", "onExit", "onExited", "onExiting"]
        , generate: 
          [ "direction", "in", "timeout"
          ]
        }
      }

    -- | TODO ThumbComponent ValueLabelComponent
    slider = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_span"
        , name: "Slider"
        , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "defaultValue" foreignType
              , Tuple "getAriaLabel" foreignType
              , Tuple "getAriaValueText" foreignType
              , eventHandlerProp "onChange"
              , eventHandlerProp "onChangeCommitted"
              , Tuple "marks" foreignType
              , Tuple "value" foreignType
              , Tuple "valueLabelFormat" foreignType
              ]
          , generate: 
              [ "aria-label" 
              , "aria-labelledby"
              , "aria-valuetext"
              , "classes"
              , "color"
              , "disabled"
              , "max"
              , "min"
              , "name"
              , "orientation"
              , "step"
              , "track"
              , "valueLabelDisplay"
              ]
          }
        }

    -- | TODO: TransitionComponent, TransitionProps
    snackbar = simpleComponent
      { inherits: Just divProps
        , name: "Snackbar"
        , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "action" jsx
              , Tuple "ClickAwayListenerProps" (Type.constructor "MUI.Core.ClickAwayListener.ClickAwayListenerProps")
              , Tuple "ContentProps" (Type.constructor "MUI.Core.SnackbarContent.SnackbarContentProps")
              -- `key` is in the docs but not in the typedef
              --, Tuple "key" foreignType 
              , Tuple "message" jsx
              , eventHandlerProp "onClose"
              , eventHandlerProp "onEnter"
              , eventHandlerProp "onEntered"
              , eventHandlerProp "onEntering"
              , eventHandlerProp "onExit"
              , eventHandlerProp "onExited"
              , eventHandlerProp "onExiting"
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
        }

    snackbarContent = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.Paper.PaperPropsOptions") [ divProps ]
        , name: "SnackbarContent"
        , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ Tuple "action" jsx
              , Tuple "message" jsx
              ]
          , generate: 
              [ "classes"
              , "role" 
              ]
          }
        }

    step = simpleComponent
      { inherits: Just divProps
        , name: "Step"
        , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "connector" jsx
              ]
          , generate: 
              [ "active" 
              , "alternativeLabel"
              , "completed"
              , "disabled"
              , "index"
              , "last"
              , "orientation"
              ]
          }
        }

    stepButton = simpleComponent
        { inherits: Just $ Type.app (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions") [ Type.constructor "React.Basic.DOM.Props_button" ]
        , name: "StepButton"
        , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "icon" foreignType
              , Tuple "optional" jsx
              ]
          , generate: 
              [ "active" 
              , "alternativeLabel"
              , "classes"
              , "completed"
              , "disabled"
              , "last"
              , "orientation"
              ]
          }
        }

    stepConnector = simpleComponent
      { inherits: Just divProps
      , name: "StepConnector"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              ]
          , generate: 
              [ "active" 
              , "alternativeLabel"
              , "classes"
              , "completed"
              , "disabled"
              , "index"
              , "orientation"
              ]
          }
        }

    -- | TODO: TransitionComponent, transitionDuration, TransitionProps
    stepContent = simpleComponent
      { inherits: Just divProps
      , name: "StepContent"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              ]
          , generate: 
              [ "active" 
              , "alternativeLabel"
              , "classes"
              , "completed"
              , "last"
              , "optional"
              , "orientation"
              ]
          }
        }


    stepIcon = simpleComponent
      { inherits: Just divProps
      , name: "StepIcon"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "icon" jsx
              ]
          , generate: 
              [ "active" 
              , "classes"
              , "completed"
              , "error"
              ]
          }
        }

    -- | TODO StepIconComponent
    stepLabel = simpleComponent
      { inherits: Just divProps
      , name: "StepLabel"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "icon" jsx
              , Tuple "optional" jsx
              , Tuple "StepIconComponent" foreignType
              , Tuple "StepIconProps" (Type.constructor "MUI.Core.StepIcon.StepIconProps")
              ]
          , generate: 
              [ "classes"
              , "disabled"
              , "error"
              ]
          }
        }

    stepper = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.Paper.PaperPropsOptions") [ divProps ]
      , name: "Stepper"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "connector" jsx
              ]
          , generate: 
              [ "activeStep"
              , "alternativeLabel"
              , "classes"
              , "nonLinear" 
              , "orientation" 
              ]
          }
        }

    svgIcon = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.SVG.Props_svg"
      , name: "SvgIcon"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
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
        }

    swipeableDrawer = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.Drawer.DrawerPropsOptions") [ divProps ]
      , name: "SwipeableDrawer"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "SwipeAreaProps" foreignType
              , eventHandlerProp "onClose"
              , eventHandlerProp "onOpen"
              ]
          , generate: 
              [ "disableBackdropTransition"
              , "disableDiscovery"
              , "disableSwipeToOpen"
              , "hysteresis"
              , "minFlingVelocity"
              , "open"
              , "swipeAreaWidth"
              ]
          }
        }

    switch = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.IconButton.IconButtonPropsOptions") [ divProps ]
      , name: "Switch"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "checkedIcon" jsx
              , Tuple "icon" jsx
              , Tuple "inputProps" foreignType
              , Tuple "inputRef" foreignType
              , eventHandlerProp "onChange"
              , Tuple "value" foreignType
              ]
          , generate: 
              [ "checked"
              , "classes"
              , "color"
              , "disabled"
              , "disableRipple"
              , "edge"
              , "id"
              , "required"
              , "size"
              , "type"
              ]
          }
        }

    tab = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions") [ Type.constructor "React.Basic.DOM.Props_button" ]
      , name: "Tab"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "icon" jsx
              , Tuple "label" jsx
              , Tuple "value" foreignType
              ]
          , generate: 
              [ "classes"
              , "disabled"
              , "disableFocusRipple"
              , "disableRipple"
              , "wrapped"
              ]
          }
        }

    table = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_table"
      , name: "Table"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              ]
          , generate: 
              [ "classes"
              , "padding"
              , "size"
              , "stickyHeader"
              ]
          }
        }

    tableBody = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_tbody"
      , name: "TableBody"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              ]
          , generate: 
              [ "classes"
              ]
          }
        }

    tableCell = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_td"
      , name: "TableCell"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              ]
          , generate: 
              [ "classes"
              , "padding"
              , "scope"
              , "size"
              --, "scopeDirection"
              , "variant"
              ]
          }
        }

    tableFooter = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_tfoot"
      , name: "TableFooter"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              ]
          , generate: 
              [ "classes"
              ]
          }
        }

    tableHead = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_thead"
      , name: "TableHead"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              ]
          , generate: 
              [ "classes"
              ]
          }
        }

    -- | TODO add TablePaginationActions
    tablePagination = simpleComponent
      { inherits: Nothing --Just $ Type.app (Type.constructor "MUI.Core.TableCell.TableCellPropsOptions") [Type.constructor "React.Basic.DOM.Props_td" ]
      , name: "TablePagination"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ 
              -- children
              -- , Tuple "backIconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonProps")
              -- , Tuple "labelDisplayedRows" foreignType
              -- , Tuple "labelRowsPerPage" jsx
              -- , Tuple "nextIconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonProps")
              -- , eventHandlerProp "onChangePage"
              -- , eventHandlerProp "onChangeRowsPerPage"
              -- , Tuple "SelectProps" (Type.constructor "MUI.Core.Select.SelecProps")
              ]
          , generate: 
              [ -- "classes"
              --, "count"
              --, "page"
              --, "rowsPerPage"
              --, "rowsPerPageOptions"
              ]
          }
        }

    tableRow = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_tr"
      , name: "TableRow"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              ]
          , generate: 
              [ "classes"
              , "hover"
              , "selected"
              ]
          }
        }

    -- | TODO IconComponent
    tableSortLabel = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions") [Type.constructor "React.Basic.DOM.Props_button" ]
      , name: "TableSortLabel"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "IconComponent" foreignType
              ]
          , generate: 
              [ "classes"
              , "active"
              , "direction"
              , "hideSortIcon"
              ]
          }
        }

    tabs = simpleComponent
      { inherits: Nothing
      , name: "Tabs"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "action" foreignType
              , eventHandlerProp "onChange"
              , Tuple "ScrollButtonComponent" foreignType
              , Tuple "TabIndicatorProps" foreignType
              , Tuple "value" foreignType
              ]
          , generate: 
              [ "classes"
              , "indicatorColor"
              , "orientation"
              , "textColor"
              , "variant" 
              , "width" 
              ]
          }
        }

    textareaAutosize = simpleComponent
      { inherits: Just $ Type.constructor "React.Basic.DOM.Props_textarea"
      , name: "TextareaAutosize"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              ]
          , generate: 
              [ "rows"
              , "rowsMax"
              ]
          }
        }

    textField = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.FormControl.FormControlPropsOptions") [ divProps ]
      , name: "TextField"
      , propsType:
        { base: basePropsRow [] $ Map.fromFoldable 
            [ children
            , Tuple "defaultValue" foreignType
            , Tuple "endAdornment" jsx
            , Tuple "helperText" jsx 
            , Tuple "InputLabelProps" (Type.constructor "MUI.Core.InputLabel.InputLabelProps")
            , Tuple "inputProps" foreignType
            , Tuple "inputRef" foreignType
            , Tuple "FormHelperTextProps" (Type.constructor "MUI.Core.FormHelperText.FormHelperTextProps")
            , Tuple "label" jsx
            , eventHandlerProp "onChange"
            , eventHandlerProp "onBlur"
            , eventHandlerProp "onFocus"
            , Tuple "SelectProps" (Type.constructor "MUI.Core.Select.SelectProps")
            , Tuple "value" foreignType
            ]
        , generate:
            [ "autoComplete"
            , "autoFocus"
            , "color"
            , "disabled"
            , "error"
            , "fullWidth"
            , "helperText"
            , "id"
            , "label"
            , "margin"
            , "multiline"
            , "name"
            , "placeholder"
            , "required"
            , "rows"
            , "rowsMax"
            , "select"
            , "type"
          ]
        }
      }

    toggleButton = simpleComponent
      { inherits: Just $ Type.app (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions") [Type.constructor "React.Basic.DOM.Props_button" ]
      , name: "ToggleButton"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              , Tuple "value" foreignType
              ]
          , generate: 
              [ "classes"
              , "disabled"
              , "disableFocusRipple"
              , "disableRipple"
              , "selected"
              ]
          }
        }

    toolbar = simpleComponent
      { inherits: Just divProps
      , name: "Toolbar"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
              ]
          , generate: 
              [ "classes"
              , "disableGutters"
              , "variant"
              ]
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

    -- | TODO needs to extend HTMLElement
    typography = simpleComponent
      { inherits: Nothing
      , name: "Typography"
      , propsType:
          { base: basePropsRow [] $ Map.fromFoldable
              [ children
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
        }

    -- | TODO needs to extend Transition
    zoom = simpleComponent
      { inherits: Nothing
      , name: "Zoom"
      , propsType:
          { base: emptyBase
          , generate: [ "in" , "timeout" ]
          }
        }

  in
    [ appBar
    , avatar
    , backdrop
    , badge
    , bottomNavigation
    , box
    , breadcrumbs
    , buttonBase
    , button
    , card
    , cardActionArea
    , cardActions
    , cardContent
    , cardHeader
    , cardMedia
    , circularProgress
    , clickAwayListener
    , checkbox
    , chip
    , collapse
    , container
    , cssBaseline
    , dialog
    , dialogActions
    , dialogContent
    , dialogTitle
    , divider
    , drawer
    , expansionPanel
    , expansionPanelActions
    , expansionPanelDetails
    , expansionPanelSummary
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
    , gridListTileBar
    , grow
    --, hidden
    , icon
    , iconButton
    , input
    , inputAdornment
    , inputBase
    , inputLabel
    , outlineInput
    , linearProgress
    , link
    , list
    , listItem
    , listItemAvatar
    , listItemText
    , listSubheader
    , menu
    , menuItem
    , modal
    , nativeSelect
    , noSsr
    , paper
    , popover
    , popper
    , portal
    , radio
    , radioGroup
    , rootRef
    , select
    , slide
    , slider
    , snackbar
    , snackbarContent
    , step
    , stepButton
    , stepConnector
    , stepContent
    , stepIcon
    , stepLabel
    , stepper
    , svgIcon
    , swipeableDrawer
    , switch
    , tab
    , table
    , tableBody
    , tableCell
    , tableFooter
    , tableHead
    --, tablePagination
    , tableRow
    , tableSortLabel
    , tabs
    , textareaAutosize
    --, textField
    --, toggleButton
    , toolbar
    , touchRipple
    , typography
    , zoom
    ]

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

commaSeparatedComponentList :: { helpText ∷ String, long ∷ String, short ∷ Char } → Parser (Array Component)
commaSeparatedComponentList { helpText, long: l, short: s } = option (multiString (Pattern ",") componentRead)
  ( long l
  <> short s
  <> metavar "component1,component2,...,component3"
  <> help helpText
  <> value [ ]
  )

showPropsOptions :: Parser Options
showPropsOptions = map ShowPropsCommand $ { component: _, skip: _ }
  <$> componentOption
  <*> commaSeparatedComponentList { helpText, long: "skip-props", short: 's' }
  where
    helpText = intercalate "."
      [ "A comma-separated list of component names "
      , "which props should be dropped from props list."
      , "This can be useful if you want to list only component own "
      , "properties or destil some inheritance etc."
      ]

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
genTarget = genComponents <|> genAllComponents <|> genIcon
  where
    genComponents =
      GenComponents <$> commaSeparatedComponentList { helpText, long: "components", short: 'c' }
      where
        helpText = "A comma-separated list of component names."

    genAllComponents = flag' GenAllComponents $
      long "all-components"
      <> short 'a'
      <> help "Codegen all components"
    genIcon = map GenIcon iconOption

genOptions :: Parser Options
genOptions = map Generate $ { target: _, output: _ }
  <$> genTarget
  <*> ((Just <$> genOutput) <|> pure Nothing)

data GenTarget
  = GenAllComponents
  | GenComponents (Array Component)
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
        Left err -> do
          log $ "\n" <> (psImportPath $ component.modulePath) <> " component codegen errors: " <> intercalate "\n" err

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
    Generate { target: GenComponents components', output } -> for_ components' \component ->
      codegenComponent component output
    Generate { target: GenAllComponents, output } -> for_ components \component ->
      codegenComponent component output
    Generate { target: GenIcon i, output } ->
      iconCodegen i output

