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
import Codegen.TS.Types (InstantiationStrategy(..))
import Control.Alt ((<|>))
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (runExceptT)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Trans.Class (lift)
import Data.Array (filter)
import Data.Array (null, sort) as Array
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
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
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

    -- | variable used For example:
    -- | type AppBarPropsOptions componentProps = (... | componentProps)
    componentPropsIdent = Ident "componentProps"

    component =
      Tuple "component"
        $ reactComponentApply
            (Type.record <<< Type.row mempty $ Just $ Left componentPropsIdent)

    defaultComponent =
      Tuple "defaultComponent"
        $ reactComponentApply
            (Type.record <<< Type.row mempty $ Just $ Left componentPropsIdent)

    basePropsRow extraVars props =
      { row: Type.row props (Just (Left componentPropsIdent))
      , vars: [ componentPropsIdent ] <> extraVars
      }

    emptyBase = basePropsRow [] mempty

    simpleComponent { optionalPropsInherits, requiredPropsInherits, name, propsType: { optionalBase, requiredBase, generate } } =
      { extraDeclarations: []
      , optionalPropsInherits
      , requiredPropsInherits
      , modulePath: Name name
      , propsType:
        { optionalBase
        , requiredBase
        , generate
        , instantiation: Nothing
        }
      , tsc: { strictNullChecks: false }
      }

    touchRippleType =
      { path: (Path "ButtonBase" (Name "TouchRipple"))
      , name: "TouchRipple"
      }

    appBar =
      simpleComponent
        { optionalPropsInherits:
          Just
            $ Type.app
                (Type.constructor "MUI.Core.Paper.PaperPropsOptions")
                [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "AppBar"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ children ]
          , requiredBase: emptyBase
          , generate: [ "classes", "color", "position" ]
          }
        }

    avatar =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "Avatar"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable []
          , requiredBase: emptyBase
          , generate:
            [ "alt"
            , "classes"
            -- not sure what to do here, "imgProps"
            , "sizes"
            , "src"
            , "srcSet"
            , "variant"
            ]
          }
        }

    backdrop =
      simpleComponent
        { optionalPropsInherits:
          Just
            $ Type.app
                (Type.constructor "MUI.Core.Fade.FadePropsOptions")
                [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "Backdrop"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "style" (Type.constructor "React.Basic.DOM.CSS")
                  ]
          , requiredBase: emptyBase
          , generate: [ "classes", "invisible", "open", "transitionDuration" ]
          }
        }

    badge =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "Badge"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "badgeContent" jsx
                  , children
                  , component
                  ]
          , requiredBase: emptyBase
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

    -- | `children` and `component` are taken from `buttonBase`
    bottomNavigation =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "BottomNavigation"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable $ [ children, component ] <> (map eventHandlerProp [ "onChange" ])
          , requiredBase: emptyBase
          , generate: [ "classes", "showLabels" ]
          }
        }

    box =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "Box"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable $ [ children, component, Tuple "css" jss ]
          , requiredBase: emptyBase
          , generate: [ "clone" ]
          }
        }

    breadcrumbs =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "Breadcrumbs"
        , propsType:
          { optionalBase:
            basePropsRow [] $ Map.fromFoldable
              $ [ children
                -- Not found on the TS side
                -- , component
                , Tuple "separator" jsx
                , Tuple "ref" foreignType
                ]
          , requiredBase: emptyBase
          , generate: [ "classes", "itemsAfterCollapse", "itemsBeforeCollapse", "maxItems" ]
          }
        }

    button =
      simpleComponent
        { optionalPropsInherits:
          Just
            $ Type.app
                (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions")
                [ Type.constructor "React.Basic.DOM.Props_button" ]
        , requiredPropsInherits: Just $ Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsRequiredOptions"
        , name: "Button"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "endIcon" jsx
                  , Tuple "startIcon" jsx
                  ]
          , requiredBase: emptyBase
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
        }

    buttonGroup =
      simpleComponent
        { optionalPropsInherits:
          Just divProps
        , requiredPropsInherits: Nothing
        , name: "ButtonGroup"
        , propsType:
          { optionalBase:
            basePropsRow [] $ Map.fromFoldable [ children ]
          , requiredBase: emptyBase
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
        }

    buttonBase =
      let
        buttonBaseActions = declType (TypeName "ButtonBaseActions") [] foreignType

        buttonBaseTypeProps = declType (TypeName "ButtonBaseTypeProp") [] foreignType
      in
        { extraDeclarations:
          [ buttonBaseActions.declaration
          , buttonBaseTypeProps.declaration
          ]
        , optionalPropsInherits:
          Just
            $ Type.app
                (Type.constructor "ButtonBasePropsOptions")
                [ Type.constructor "React.Basic.DOM.Props_button" ]
        , requiredPropsInherits: Nothing
        , modulePath: Name "ButtonBase"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "action" foreignType
                  , Tuple "buttonRef" foreignType
                  -- XXX: We are catching material ui documentation / type error here.
                  -- ButtonBase doesn't contain `component` prop.
                  -- , component
                  , eventHandlerProp "onFocusVisible"
                  -- | XXX: Provide some sugar for generating relative imports
                  -- | between components
                  , Tuple "TouchRippleProps" $ roll
                      $ TypeConstructor
                          { moduleName: Just $ ModuleName (psImportPath (componentFullPath touchRipple))
                          , name: TypeName $ (propsTypeName touchRippleType.name)
                          }
                  ]
          , requiredBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
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
          , instantiation: Nothing
          }
        , tsc: { strictNullChecks: false }
        }

    -- | TODO: make value a type variable
    bottomNavigationAction =
      simpleComponent
        { optionalPropsInherits:
          Just
            $ Type.app
                (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions")
                [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "BottomNavigationAction"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "icon" jsx
                  , Tuple "label" jsx
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "showLabel"
            , "selected"
            ]
          }
        }

    card =
      simpleComponent
        { optionalPropsInherits:
          Just
            $ Type.app
                (Type.constructor "MUI.Core.Paper.PaperPropsOptions")
                [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "Card"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ children ]
          , requiredBase: emptyBase
          , generate: [ "classes", "raised" ]
          }
        }

    cardActionArea =
      simpleComponent
        { optionalPropsInherits:
          Just
            $ Type.app
                (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions")
                [ Type.constructor "React.Basic.DOM.Props_button" ]
        , requiredPropsInherits: Nothing
        , name: "CardActionArea"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ children ]
          , requiredBase: emptyBase
          , generate: [ "classes" ]
          }
        }

    cardActions =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "CardActions"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ children ]
          , requiredBase: emptyBase
          , generate: [ "classes", "disableSpacing" ]
          }
        }

    cardContent =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "CardContent"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ children, component ]
          , requiredBase: emptyBase
          , generate: [ "classes" ]
          }
        }

    cardHeader =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "CardHeader"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "action" jsx
                  , Tuple "avatar" jsx
                  , children
                  , component
                  , Tuple "subheader" jsx
                  , Tuple "subheaderTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyProps")
                  , Tuple "title" jsx
                  , Tuple "titleTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyProps")
                  ]
          , requiredBase: emptyBase
          , generate: [ "classes", "disableTypography" ]
          }
        }

    cardMedia =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "CardMedia"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  -- This isn't being found in the .d.ts
                  --, component
                  ]
          , requiredBase: emptyBase
          , generate: [ "classes", "image", "src" ]
          }
        }

    checkbox =
      simpleComponent
        { optionalPropsInherits: Nothing -- should be IconButon
        , requiredPropsInherits: Nothing
        , name: "Checkbox"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  ( [ Tuple "checkedIcon" jsx
                    , Tuple "icon" jsx
                    , Tuple "indeterminateIcon" jsx
                    , Tuple "inputProps" foreignType
                    , Tuple "inputRef" foreignType
                    , Tuple "value" foreignType
                    ]
                      <> (map eventHandlerProp [ "onChange" ])
                  )
          , requiredBase: emptyBase
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

    chip =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "Chip"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  ( [ Tuple "avatar" jsx
                    , Tuple "deleteIcon" jsx
                    , Tuple "icon" jsx
                    , Tuple "label" jsx
                    ]
                      <> (map eventHandlerProp [ "onDelete" ])
                  )
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "color"
            , "disabled"
            , "size"
            , "variant"
            ]
          }
        }

    circularProgress =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "CircularProgress"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable []
          , requiredBase: emptyBase
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

        optionalBase = basePropsRow [] $ Map.fromFoldable [ child, onClickAway ]
      in
        simpleComponent
          { optionalPropsInherits: Nothing
          , requiredPropsInherits: Nothing
          , name: "ClickAwayListener"
          , propsType:
            { optionalBase
            , requiredBase: emptyBase
            , generate: [ "mouseEvent", "touchEvent" ]
            }
          }

    collapse =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "Collapse"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ children, component ]
          , requiredBase: emptyBase
          , generate:
            [ "collapsedHeight"
            , "in"
            , "timeout"
            ]
          }
        }

    container =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "Container"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ component ]
          , requiredBase: emptyBase
          , generate:
            [ "fixed"
            , "maxWidth"
            ]
          }
        }

    cssBaseline =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "CssBaseline"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ children ]
          , requiredBase: emptyBase
          , generate: []
          }
        }

    dialog =
      let
        -- | TODO:
        -- | migration.
        -- | * `PaperComponent`, `PaperProps`, `TransitionComponent`, `TransitionDuration`
        handlers =
          map eventHandlerProp
            [ "onEnter"
            , "onEntered"
            , "onEntering"
            , "onExit"
            , "onExited"
            , "onExiting"
            ]

        optionalBase = basePropsRow [] $ Map.fromFoldable $ [ children ] <> handlers
      in
        simpleComponent
          { optionalPropsInherits:
            Just
              $ Type.app
                  (Type.constructor "MUI.Core.Modal.ModalPropsOptions")
                  [ divProps ]
          , requiredPropsInherits: Nothing
          , name: "Dialog"
          , propsType:
            { optionalBase
            , requiredBase: emptyBase
            , generate:
              [ "aria-describedby"
              , "aria-labelledby"
              , "classes"
              , "fullScreen"
              , "fullWidth"
              , "maxWidth"
              , "scroll"
              , "transitionDuration"
              ]
            }
          }

    dialogActions =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "DialogActions"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ children ]
          , requiredBase: emptyBase
          , generate: [ "classes", "disableSpacing" ]
          }
        }

    dialogContent =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "DialogContent"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ children ]
          , requiredBase: emptyBase
          , generate: [ "classes", "dividers" ]
          }
        }

    dialogTitle =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "DialogTitle"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ children ]
          , requiredBase: emptyBase
          , generate: [ "classes", "disableTypography" ]
          }
        }

    -- | TODO: add component
    divider =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_hr"
        , requiredPropsInherits: Nothing
        , name: "Divider"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable []
          , requiredBase: emptyBase
          , generate:
            [ "absolute"
            , "classes"
            , "light"
            , "orientation"
            , "variant"
            ]
          }
        }

    drawer =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.Modal.ModalPropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "Drawer"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  ( [ children
                    , Tuple "ModalProps" (Type.constructor "MUI.Core.Modal.ModalPropsPartial")
                    , eventHandlerProp "onClose"
                    , Tuple "PaperProps" (Type.constructor "MUI.Core.Modal.ModalPropsPartial")
                    , Tuple "SlideProps" (Type.constructor "MUI.Core.Slide.SlidePropsPartial")
                    ]
                      <> map eventHandlerProp [ "onClose", "onEnter", "onEntered", "onEntering", "onExit", "onExited", "onExiting" ]
                  )
          , requiredBase: emptyBase
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
    expansionPanel =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.Paper.PaperPropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "ExpansionPanel"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , eventHandlerProp "onChange"
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "defaultExpanded"
            , "disabled"
            , "expanded"
            ]
          }
        }

    expansionPanelActions =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "ExpansionPanelActions"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            ]
          }
        }

    expansionPanelDetails =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "ExpansionPanelDetails"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            ]
          }
        }

    expansionPanelSummary =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "ExpansionPanelSummary"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "expandIcon" jsx
                  , Tuple "IconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonPropsPartial")
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            ]
          }
        }

    fab =
      simpleComponent
        { optionalPropsInherits:
          Just
            $ Type.app
                (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions")
                [ Type.constructor "React.Basic.DOM.Props_button" ]
        , requiredPropsInherits: Nothing
        , name: "Fab"
        , propsType:
          { optionalBase: emptyBase
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "color"
            , "disabled"
            , "disableFocusRipple"
            , "href"
            , "size"
            , "variant"
            ]
          }
        }

    -- | TODO: TransitionComponent
    fade =
      simpleComponent
        { optionalPropsInherits: Nothing -- should inherit TransitionComponent
        , requiredPropsInherits: Nothing
        , name: "Fade"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ Tuple "ref" foreignType ]
          , requiredBase: emptyBase
          , generate:
            [ {-- not sure what to do here, "theme" --}]
          }
        }

    -- | TODO: inputComponent, make value a type variable
    filledInput =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.InputBasePropsOption") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "FilledInput"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "endAdornment" jsx
                  , Tuple "inputProps" (Type.constructor "MUI.Core.InputBasePartial")
                  , Tuple "inputRef" foreignType
                  , eventHandlerProp "onChange"
                  , Tuple "startAdornment" jsx
                  , Tuple "value" foreignType
                  ]
          , requiredBase: emptyBase
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

    formControl =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "FormControl"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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

    -- | TODO: make value a type variable
    formControlLabel =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_label"
        , requiredPropsInherits: Nothing
        , name: "FormControlLabel"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "control" jsx
                  , Tuple "label" jsx
                  , eventHandlerProp "onChange"
                  , Tuple "value" foreignType
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "checked"
            , "classes"
            , "disabled"
            , "labelPlacement"
            , "name"
            ]
          }
        }

    formGroup =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "FormGroup"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "row"
            ]
          }
        }

    formHelperText =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_p"
        , requiredPropsInherits: Nothing
        , name: "FormHelperText"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , component
                  ]
          , requiredBase: emptyBase
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

    formLabel =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_label"
        , requiredPropsInherits: Nothing
        , name: "FormLabel"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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

    grid =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "Grid"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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
        optionalBase =
          basePropsRow []
            $ Map.fromFoldable
                [ children
                , component
                ]
      in
        simpleComponent
          { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_ul"
          , requiredPropsInherits: Nothing
          , name: "GridList"
          , propsType:
            { optionalBase
            , requiredBase: emptyBase
            , generate: [ "cellHeight", "classes", "cols", "spacing" ]
            }
          }

    gridListTile =
      let
        optionalBase =
          basePropsRow []
            $ Map.fromFoldable
                [ children
                , component
                ]
      in
        simpleComponent
          { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_li"
          , requiredPropsInherits: Nothing
          , name: "GridListTile"
          , propsType:
            { optionalBase
            , requiredBase: emptyBase
            , generate: [ "classes", "cols", "rows" ]
            }
          }

    gridListTileBar =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "GridListTileBar"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "actionIcon" jsx
                  , Tuple "subtitle" jsx
                  , Tuple "title" jsx
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "actionPosition"
            , "titlePosition"
            ]
          }
        }

    -- | TODO: update when Transition is figured out
    grow =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "Grow"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  []
          , requiredBase: emptyBase
          , generate:
            [ "in"
            , "timeout"
            ]
          }
        }

    hidden =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "Hidden"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable [ Tuple "only" foreignType ]
          , requiredBase: emptyBase
          , generate:
            [ "implementation"
            , "initialWidth"
            , "lgDown"
            , "lgUp"
            , "mdDown"
            , "mdUp"
            , "smDown"
            , "smUp"
            , "xlDown"
            , "xlUp"
            , "xsDown"
            , "xsUp"
            ]
          }
        }

    icon =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_span"
        , requiredPropsInherits: Nothing
        , name: "Icon"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ component
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "color"
            , "fontSize"
            ]
          }
        }

    iconButton =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "IconButton"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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

    input =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.InputBase.InputBasePropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "Input"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "defaultValue" foreignType
                  , Tuple "endAdornment" jsx
                  , Tuple "inputProps" foreignType
                  , Tuple "inputRef" foreignType
                  , Tuple "startAdornment" jsx
                  , Tuple "value" foreignType
                  , eventHandlerProp "onChange"
                  ]
          , requiredBase: emptyBase
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

    inputAdornment =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "InputAdornment"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , component
                  ]
          , requiredBase: emptyBase
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
    inputBase =
      simpleComponent
        { optionalPropsInherits: Just $ divProps
        , requiredPropsInherits: Nothing
        , name: "InputBase"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "defaultValue" foreignType
                  , Tuple "endAdornment" jsx
                  , Tuple "inputProps" foreignType
                  , Tuple "inputRef" foreignType
                  , Tuple "startAdornment" jsx
                  , Tuple "value" foreignType
                  , eventHandlerProp "onChange"
                  ]
          , requiredBase: emptyBase
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

    inputLabel =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.FormLabel.FormLabelPropsOptions") [ Type.constructor "React.Basic.DOM.Props_label" ]
        , requiredPropsInherits: Nothing
        , name: "InputLabel"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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

    linearProgress =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "LinearProgress"
        , propsType:
          { optionalBase: emptyBase
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "color"
            , "value"
            , "valueBuffer"
            , "variant"
            ]
          }
        }

    link =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_a"
        , requiredPropsInherits: Nothing
        , name: "Link"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "TypographyClasses" (Type.constructor "MUI.Core.Typography.TypographyClassKey")
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "color"
            , "underline"
            , "variant"
            ]
          }
        }

    list =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_ul"
        , requiredPropsInherits: Nothing
        , name: "List"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "subheader" jsx
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "dense"
            , "disablePadding"
            ]
          }
        }

    -- | TODO: add ContainerComponent and ContainerProps
    listItem =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_li"
        , requiredPropsInherits: Nothing
        , name: "ListItem"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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

    listItemAvatar =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "ListItemAvatar"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            ]
          }
        }

    listItemIcon =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "ListItemIcon"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            ]
          }
        }

    listItemSecondaryAction =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "ListItemSecondaryAction"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            ]
          }
        }

    listItemText =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "ListItemText"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "primary" jsx
                  , Tuple "primaryTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyClassKey")
                  , Tuple "secondary" jsx
                  , Tuple "secondaryTypographyProps" (Type.constructor "MUI.Core.Typography.TypographyClassKey")
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "disableTypography"
            , "inset"
            ]
          }
        }

    listSubheader =
      simpleComponent
        { optionalPropsInherits: Just $ (Type.constructor "React.Basic.DOM.Props_li")
        , requiredPropsInherits: Nothing
        , name: "ListSubheader"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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
        handlers =
          map eventHandlerProp
            [ "onClose", "onEnter", "onEntered", "onEntering", "onExit", "onExited", "onExiting" ]

        -- | I'm not sure what is the difference between `React.Element` and `DOM.Element`
        nullable = Type.constructor "Data.Nullable.Nullable"

        domElement = Type.constructor "Web.DOM.Element"

        anchorEl = Tuple "anchorEl" $ Type.app nullable [ domElement ]

        optionalBase = basePropsRow [] $ Map.fromFoldable $ [ anchorEl, children ] <> handlers
      in
        simpleComponent
          { optionalPropsInherits: Nothing -- | We should inherit from Popover here
          , requiredPropsInherits: Nothing
          , name: "Menu"
          , propsType:
            { optionalBase
            , requiredBase: emptyBase
            , generate: [ "autoFocus", "classes", "disableAutoFocusItem", "open", "transitionDuration", "variant" ]
            }
          }

    menuItem =
      let
        optionalBase =
          basePropsRow []
            $ Map.fromFoldable
                [ children
                -- XXX: We are catching material ui documentation / type error here.
                -- MenuItem doesn't contain `component` prop.
                -- , component
                ]
      in
        simpleComponent
          { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.ListItem.ListItemPropsOptions") [ Type.constructor "React.Basic.DOM.Props_li" ]
          , requiredPropsInherits: Nothing
          , name: "MenuItem"
          , propsType:
            { optionalBase
            , requiredBase: emptyBase
            , generate: [ "classes", "dense", "disableGutters" ]
            }
          }

    mobileStepper =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.Paper.PaperPropsOptions") [ Type.constructor "React.Basic.DOM.Props_props" ]
        , requiredPropsInherits: Nothing
        , name: "MobileStepper"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "backButton" jsx
                  , Tuple "LinearProgressProps" (Type.constructor "MUI.Core.LinearPropgress.LinearProgressProps")
                  , Tuple "nextButton" jsx
                  ]
          , requiredBase: emptyBase
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
        handlers =
          map eventHandlerProp
            [ "onBackdropClick"
            , "onClose"
            , "onEscapeKeyDown"
            , "onRendered"
            ]

        backdropPropsPartial = Type.constructor "MUI.Core.Backdrop.BackdropPropsPartial"

        optionalBase =
          basePropsRow [] $ Map.fromFoldable
            $ [ children
              , Tuple "BackdropComponent" (reactComponentApply backdropPropsPartial)
              , Tuple "BackdropProps" backdropPropsPartial
              -- , container
              , Tuple
                  "manager"
                  (Type.constructor "MUI.Core.Modal.ModalManager.ModalManager")
              ]
            <> handlers
      in
        simpleComponent
          { optionalPropsInherits: Nothing
          , requiredPropsInherits: Nothing
          , name: "Modal"
          , propsType:
            { optionalBase
            , requiredBase: emptyBase
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

    -- | TODO: value
    nativeSelect =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.Input.InputPropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "NativeSelect"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "IconComponent" jsx
                  , Tuple "input" jsx
                  , Tuple "inputProps" (Type.constructor "MUI.Core.Input.InputProps")
                  , eventHandlerProp "onChange"
                  , Tuple "value" foreignType
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "variant"
            ]
          }
        }

    -- | TODO: value
    noSsr =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "NoSsr"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "fallback" jsx
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "defer"
            ]
          }
        }

    -- | inputComponent
    outlineInput =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.InputBase.InputBasePropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "OutlinedInput"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "defaultValue" foreignType
                  , Tuple "endAdornment" jsx
                  , Tuple "inputProps" foreignType
                  , Tuple "inputRef" foreignType
                  , Tuple "startAdornment" jsx
                  , Tuple "value" foreignType
                  , eventHandlerProp "onChange"
                  ]
          , requiredBase: emptyBase
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

    paper =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "Paper"
        , propsType:
          { optionalBase: basePropsRow [] $ Map.fromFoldable $ [ children, component ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "elevation"
            , "square"
            ]
          }
        }

    -- | TransitionComponent, TransitionProps, transformOrigin
    popover =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.Modal.ModalPropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "Popover"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
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
          , requiredBase: emptyBase
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

    popper =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "Popper"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "anchorEl" foreignType
                  , children
                  , Tuple "modifiers" foreignType
                  , Tuple "popperOptions" foreignType
                  , Tuple "popperRef" foreignType
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "disablePortal"
            , "keepMounted"
            , "open"
            , "transition"
            ]
          }
        }

    portal =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "Portal"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "container" foreignType
                  , eventHandlerProp "onRendered"
                  ]
          , requiredBase: emptyBase
          , generate: [ "disablePortal" ]
          }
        }

    -- | value, inputProps, inputRef
    radio =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.IconButton.IconButtonPropsOptions") [ Type.constructor "React.Basic.DOM.Props_button" ]
        , requiredPropsInherits: Nothing
        , name: "Radio"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "checkedIcon" jsx
                  , Tuple "icon" jsx
                  , Tuple "inputProps" foreignType
                  , Tuple "inputRef" foreignType
                  , eventHandlerProp "onChange"
                  , Tuple "value" foreignType
                  ]
          , requiredBase: emptyBase
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
    radioGroup =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.FormGroup.FormGroupPropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "RadioGroup"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "defaultValue" foreignType
                  , eventHandlerProp "onChange"
                  , Tuple "value" foreignType
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "name"
            ]
          }
        }

    rootRef =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "RootRef"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "rootRef" foreignType
                  ]
          , requiredBase: emptyBase
          , generate:
            []
          }
        }

    -- | TODO: value
    select =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.Input.InputPropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "Select"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
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
          , requiredBase: emptyBase
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

    slide =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "Slide"
        , propsType:
          { optionalBase:
            basePropsRow [] $ Map.fromFoldable
              $ map eventHandlerProp [ "onEnter", "onEntered", "onEntering", "onExit", "onExited", "onExiting" ]
          , requiredBase: emptyBase
          , generate:
            [ "direction", "in", "timeout"
            ]
          }
        }

    -- | TODO: ThumbComponent ValueLabelComponent
    slider =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_span"
        , requiredPropsInherits: Nothing
        , name: "Slider"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
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
          , requiredBase: emptyBase
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
    snackbar =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "Snackbar"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
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
          , requiredBase: emptyBase
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

    snackbarContent =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.Paper.PaperPropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "SnackbarContent"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ Tuple "action" jsx
                  , Tuple "message" jsx
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "role"
            ]
          }
        }

    step =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "Step"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "connector" jsx
                  ]
          , requiredBase: emptyBase
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

    stepButton =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions") [ Type.constructor "React.Basic.DOM.Props_button" ]
        , requiredPropsInherits: Nothing
        , name: "StepButton"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "icon" foreignType
                  , Tuple "optional" jsx
                  ]
          , requiredBase: emptyBase
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

    stepConnector =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "StepConnector"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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
    stepContent =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "StepContent"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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

    stepIcon =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "StepIcon"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "icon" jsx
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "active"
            , "classes"
            , "completed"
            , "error"
            ]
          }
        }

    -- | TODO: StepIconComponent
    stepLabel =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "StepLabel"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "icon" jsx
                  , Tuple "optional" jsx
                  , Tuple "StepIconComponent" foreignType
                  , Tuple "StepIconProps" (Type.constructor "MUI.Core.StepIcon.StepIconProps")
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "disabled"
            , "error"
            ]
          }
        }

    stepper =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.Paper.PaperPropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "Stepper"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "connector" jsx
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "activeStep"
            , "alternativeLabel"
            , "classes"
            , "nonLinear"
            , "orientation"
            ]
          }
        }

    svgIcon =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.SVG.Props_svg"
        , requiredPropsInherits: Nothing
        , name: "SvgIcon"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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

    swipeableDrawer =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.Drawer.DrawerPropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "SwipeableDrawer"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "SwipeAreaProps" foreignType
                  , eventHandlerProp "onClose"
                  , eventHandlerProp "onOpen"
                  ]
          , requiredBase: emptyBase
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

    switch =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.IconButton.IconButtonPropsOptions") [ divProps ]
        , requiredPropsInherits: Nothing
        , name: "Switch"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "checkedIcon" jsx
                  , Tuple "icon" jsx
                  , Tuple "inputProps" foreignType
                  , Tuple "inputRef" foreignType
                  , eventHandlerProp "onChange"
                  , Tuple "value" foreignType
                  ]
          , requiredBase: emptyBase
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

    tab =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions") [ Type.constructor "React.Basic.DOM.Props_button" ]
        , requiredPropsInherits: Nothing
        , name: "Tab"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "icon" jsx
                  , Tuple "label" jsx
                  , Tuple "value" foreignType
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "disabled"
            , "disableFocusRipple"
            , "disableRipple"
            , "fullWidth"
            , "selected"
            , "wrapped"
            ]
          }
        }

    table =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_table"
        , requiredPropsInherits: Nothing
        , name: "Table"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "padding"
            , "size"
            , "stickyHeader"
            ]
          }
        }

    tableBody =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_tbody"
        , requiredPropsInherits: Nothing
        , name: "TableBody"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            ]
          }
        }

    tableCell =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_td"
        , requiredPropsInherits: Nothing
        , name: "TableCell"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
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

    tableFooter =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_tfoot"
        , requiredPropsInherits: Nothing
        , name: "TableFooter"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            ]
          }
        }

    tableHead =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_thead"
        , requiredPropsInherits: Nothing
        , name: "TableHead"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            ]
          }
        }

    -- | TODO: add TablePaginationActions
    tablePagination :: Component
    tablePagination =
      { extraDeclarations: []
      , optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.TableCell.TableCellPropsOptions") [ Type.constructor "React.Basic.DOM.Props_td" ]
      , requiredPropsInherits: Nothing
      , modulePath: Name "TablePagination"
      , propsType:
        { optionalBase:
          basePropsRow []
            $ Map.fromFoldable
                [ children
                , Tuple "backIconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonProps")
                , Tuple "labelDisplayedRows" foreignType
                , Tuple "labelRowsPerPage" jsx
                , Tuple "nextIconButtonProps" (Type.constructor "MUI.Core.IconButton.IconButtonProps")
                , eventHandlerProp "onChangePage"
                , eventHandlerProp "onChangeRowsPerPage"
                , Tuple "SelectProps" (Type.constructor "MUI.Core.Select.SelectProps")
                ]
        , requiredBase: emptyBase
        , generate:
          [ "classes"
          , "count"
          , "page"
          , "rowsPerPage"
          , "rowsPerPageOptions"
          ]
        -- | Long story short `TablePaginationProps` is a union
        -- | so we are not able to create an interface instance for it.
        -- | Fortunately we are able to extract interesting props
        -- | from the first part component of this union...
        , instantiation:
          Just
            { strategy: TypeAlias
            , extractProps:
              \defaultInstance -> case unroll defaultInstance of
                (Instantiation.Union [ Mu.In (Instantiation.Object fqn props), _ ]) -> pure { fqn, props }
                otherwise ->
                  throwError
                    [ "Expecting an union as a representation for TablePaginationProps" ]
            }
        }
      , tsc: { strictNullChecks: false }
      }

    tableRow =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_tr"
        , requiredPropsInherits: Nothing
        , name: "TableRow"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "hover"
            , "selected"
            ]
          }
        }

    -- | TODO: IconComponent
    tableSortLabel =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions") [ Type.constructor "React.Basic.DOM.Props_button" ]
        , requiredPropsInherits: Nothing
        , name: "TableSortLabel"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "IconComponent" foreignType
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "active"
            , "direction"
            , "hideSortIcon"
            ]
          }
        }

    tabs =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "Tabs"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "action" foreignType
                  , eventHandlerProp "onChange"
                  , Tuple "ScrollButtonComponent" foreignType
                  , Tuple "TabIndicatorProps" foreignType
                  , Tuple "value" foreignType
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "centered"
            , "classes"
            , "indicatorColor"
            , "orientation"
            , "scrollButtons"
            , "textColor"
            , "variant"
            , "width"
            ]
          }
        }

    textareaAutosize =
      simpleComponent
        { optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_textarea"
        , requiredPropsInherits: Nothing
        , name: "TextareaAutosize"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "rows"
            , "rowsMax"
            ]
          }
        }

    textField :: Component
    textField =
      { extraDeclarations: []
      , optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.FormControl.FormControlPropsOptions") [ divProps ]
      , requiredPropsInherits: Nothing
      , modulePath: Name "TextField"
      , propsType:
        { optionalBase:
          basePropsRow []
            $ Map.fromFoldable
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
        , requiredBase: emptyBase
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
        , instantiation:
          Just
            { strategy: TypeAlias
            , extractProps:
              \defaultInstance -> case unroll defaultInstance of
                ( Instantiation.Union
                    [ Mu.In (Instantiation.Object fqn1 props1)
                  , Mu.In (Instantiation.Object fqn2 props2)
                  , Mu.In (Instantiation.Object fqn3 props3)
                  ]
                ) ->
                  throwError
                    [ "Not sure how to tackle this three props types: " <> fqn1 <> ", " <> fqn2 <> "," <> fqn3 ]
                (Instantiation.Union _) ->
                  throwError
                    [ "Expecting a two member union as a representation for TextField" ]
                otherwise ->
                  throwError
                    [ "Expecting an union as a representation for TextField" ]
            }
        }
      , tsc: { strictNullChecks: false }
      }

    -- | It seems that toggle button is still in `material-ui-lab`
    toggleButton =
      simpleComponent
        { optionalPropsInherits: Just $ Type.app (Type.constructor "MUI.Core.ButtonBase.ButtonBasePropsOptions") [ Type.constructor "React.Basic.DOM.Props_button" ]
        , requiredPropsInherits: Nothing
        , name: "ToggleButton"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , Tuple "value" foreignType
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "disabled"
            , "disableFocusRipple"
            , "disableRipple"
            , "selected"
            ]
          }
        }

    toolbar =
      simpleComponent
        { optionalPropsInherits: Just divProps
        , requiredPropsInherits: Nothing
        , name: "Toolbar"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  ]
          , requiredBase: emptyBase
          , generate:
            [ "classes"
            , "disableGutters"
            , "variant"
            ]
          }
        }

    touchRipple =
      { extraDeclarations: []
      , optionalPropsInherits: Just $ Type.constructor "React.Basic.DOM.Props_span"
      , requiredPropsInherits: Nothing
      -- , name: touchRippleType.name
      , modulePath: touchRippleType.path
      , propsType:
        { optionalBase: emptyBase
        , requiredBase: emptyBase
        , generate: [ "center", "classes" ]
        , instantiation: Nothing
        }
      , tsc: { strictNullChecks: false }
      }

    -- | TODO: needs to extend HTMLElement
    typography =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "Typography"
        , propsType:
          { optionalBase:
            basePropsRow []
              $ Map.fromFoldable
                  [ children
                  , component
                  ]
          , requiredBase: emptyBase
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

    -- | TODO: needs to extend Transition
    zoom =
      simpleComponent
        { optionalPropsInherits: Nothing
        , requiredPropsInherits: Nothing
        , name: "Zoom"
        , propsType:
          { optionalBase: emptyBase
          , requiredBase: emptyBase
          , generate: [ "in", "timeout" ]
          }
        }
  in
    [ appBar
    , avatar
    , backdrop
    , badge
    , bottomNavigation
    , bottomNavigationAction
    , box
    , breadcrumbs
    , buttonBase
    , buttonGroup
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
    , hidden
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
    , listItemIcon
    , listItemSecondaryAction
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
    , tablePagination
    , tableRow
    , tableSortLabel
    , tabs
    , textareaAutosize
    -- , textField
    -- , toggleButton
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
  step c = Tuple (Model.componentName c) c

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
    <*> ((Just <$> genOutput) <|> pure Nothing)

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
    -- | Should I bring back multimodule handling logic?
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
      runExceptT (Codegen.component component)
        >>= case _ of
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
      runExceptT getProps
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
