module Examples.Main where

import Prelude
import Data.Array (elem, singleton) as Array
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Exception (throw)
import Effect.Unsafe (unsafePerformEffect)
import Foreign (unsafeToForeign)
import MUI.Core (jss, mediaQuery)
import MUI.Core.AppBar (appBarWithStyles)
import MUI.Core.AppBar (position, props) as AppBar
import MUI.Core.Backdrop (_UnsafeBackdrop)
import MUI.Core.Badge (badge, badgeWithStyles)
import MUI.Core.Badge (color) as Badge
import MUI.Core.Box (box)
import MUI.Core.Button (button, buttonWithStyles)
import MUI.Core.Button (button, color, props) as Button
import MUI.Core.ButtonGroup (buttonGroup)
import MUI.Core.ButtonGroup (buttonGroupWithStyles, color, variant) as ButtonGroup
import MUI.Core.Container (container)
import MUI.Core.CssBaseline (cssBaseline)
import MUI.Core.Divider (divider, dividerWithStyles)
import MUI.Core.Divider (variant) as Dividier
import MUI.Core.Drawer (anchor) as Drawer
import MUI.Core.Drawer (drawer)
import MUI.Core.Fade (fade)
import MUI.Core.FormControl (formControlWithStyles)
import MUI.Core.FormControl (props) as FormControl
import MUI.Core.FormHelperText (formHelperText)
import MUI.Core.Grid (grid)
import MUI.Core.Grid (gridSize) as Grid
import MUI.Core.Hidden (hidden)
import MUI.Core.Hidden (implementation, only) as Hidden
import MUI.Core.Input (input)
import MUI.Core.InputLabel (inputLabel)
import MUI.Core.Link (color) as Link
import MUI.Core.Link (link)
import MUI.Core.List (list)
import MUI.Core.ListItem (listItem)
import MUI.Core.ListItemIcon (listItemIcon)
import MUI.Core.ListItemText (listItemText)
import MUI.Core.Modal (modal)
import MUI.Core.Styles (Theme)
import MUI.Core.Styles (md) as Styles
import MUI.Core.Styles.CreateMuiTheme (createMuiTheme)
import MUI.Core.Styles.CreatePalette (paletteOptions)
import MUI.Core.Styles.MakeStyles (makeStyles)
import MUI.Core.Styles.MuiThemeProvider (muiThemeProvider)
import MUI.Core.Styles.Types (multiplier, spacing)
import MUI.Core.TextField (filledWithStyles, outlinedWithStyles, standardWithStyles)
import MUI.Core.TextField (filledWithStyles, outlinedWithStyles, standardWithStyles) as TextField
import MUI.Core.TextField.FilledTextField (props) as FilledTextField
import MUI.Core.TextField.OutlinedTextField (props) as OutlinedWithStyles
import MUI.Core.TextField.StandardTextField (props) as StandardTextField
import MUI.Core.Toolbar (toolbar)
import MUI.Core.Typography (typography)
import MUI.Core.Typography (variant) as Typography
import MUI.Icons (icon, iconWithStyles)
import MUI.Icons (props) as Icons
import MUI.Icons.Inbox (inbox)
import MUI.Icons.Mail (mail)
import MUI.Icons.Menu (menu)
import MUI.React.TransitionGroup (single) as TransitionGroup
import MUI.System.Display (flex, none) as Only
import MUI.System.Display (hiding)
import MUI.System.Flexbox.JustifyContent (flexEnd) as JustifyContent
import React.Basic (Component, JSX, ReactComponent, createComponent, element, fragment, make)
import React.Basic.DOM (a, button, div, div_, form, h2, p, span, text) as DOM
import React.Basic.DOM (render)
import React.Basic.Events (handler_)
import React.Basic.Hooks (useState)
import React.Basic.Hooks as React
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

type Props
  = {}

theme :: Theme
theme =
  createMuiTheme
    { palette: paletteOptions { type: "dark" } }

component :: Component Props
component = createComponent "Counter"

gridItem :: JSX -> JSX
gridItem child = grid { item: true, children: [ child ], xs: Grid.gridSize.six }

arr :: forall a. a -> Array a
arr = Array.singleton

-- | Based on: https://github.com/mui-org/material-ui/blob/master/docs/src/pages/components/drawers/TemporaryDrawer.js
drawerList :: Effect (ReactComponent {})
drawerList =
  let
    useStyles =
      makeStyles \t ->
        { paper:
            { backgroundColor: theme.palette.background.paper
            , padding:
                spacing
                  (multiplier 2.0)
                  (multiplier 4.0)
                  (multiplier 3.0)
                  (multiplier 4.0)
                  t
            }
        , list: { width: 250 }
        , fullList: { width: "auto" }
        }

    menuList classes anchor =
      DOM.div
        $ { className:
              if anchor `Array.elem` [ Drawer.anchor.bottom, Drawer.anchor.top ] then
                classes.fullList
              else
                classes.list
          , children: _
          , role: "presentation"
          }
        $ arr
        <<< list
        $ { children: _ }
        $ ( [ "Inbox", "Starred", "Send emai", "Drafts" ]
              <#> \text ->
                  listItem
                    $ { button: true, children: _ }
                        [ listItemIcon { children: [ icon (if text == "Inbox" then inbox else mail) {} ] }
                        , listItemText { primary: DOM.text text }
                        ]
          )
        <> [ divider {} ]
        <> ( [ "All mail", "Trash", "Spam" ]
              <#> \text ->
                  listItem
                    $ { button: true, children: _ }
                        [ listItemIcon { children: [ icon (if text == "Inbox" then inbox else mail) {} ] }
                        , listItemText { primary: DOM.text text }
                        ]
          )
  in
    React.component "TemporaryDrawer" \_ -> React.do
      state /\ setState <- useState Nothing
      classes <- useStyles
      let
        -- In the original example we have here also this check
        -- if (event.type === 'keydown' && (event.key === 'Tab' || event.key === 'Shift')) {
        --    return;
        --  }
        openDrawer a = setState (const $ Just a)

        closeDrawer = setState (const Nothing)
      pure $ DOM.div_ $ [ Drawer.anchor.left, Drawer.anchor.right, Drawer.anchor.top, Drawer.anchor.bottom ]
        <#> \anchor ->
            DOM.div
              $ { key: unsafeCoerce anchor, children: _ }
                  [ button
                      { onClick: handler_ (openDrawer anchor)
                      , children: [ DOM.text (unsafeCoerce anchor) ]
                      }
                  , drawer
                      $ { anchor
                        , open: Just anchor == state
                        , onClose: handler_ closeDrawer
                        , children: _
                        }
                      $ [ menuList classes anchor ]
                  ]

-- | MUI example: https://github.com/mui-org/material-ui/blob/master/docs/src/pages/components/modal/TransitionsModal.js
transitionsModal :: Effect (ReactComponent {})
transitionsModal =
  let
    useStyles =
      makeStyles \t ->
        { modal:
            { display: "flex"
            , alignItems: "center"
            , justifyContent: "center"
            }
        , paper:
            { backgroundColor: t.palette.background.paper
            , border: "2px solid #000"
            , boxShadow: theme.shadows."24"
            , padding:
                spacing
                  (multiplier 2.0)
                  (multiplier 4.0)
                  (multiplier 3.0)
                  (multiplier 4.0)
                  t
            }
        }
  in
    React.component "TransitionsModal" \_ -> React.do
      open /\ setOpen <- useState false
      classes <- useStyles
      let
        handleOpen = handler_ $ setOpen (const true)

        handleClose = handler_ $ setOpen (const false)
      pure
        $ DOM.div_
            [ DOM.button
                $ { children: [ DOM.text "Modal: react-transition-group" ]
                  , type: "button"
                  , onClick: handleOpen
                  }
            , modal
                $ { "BackdropProps":
                      unsafeToForeign { transitionDuration: TransitionGroup.single 500.0 }
                  , "BackdropComponent":
                      unsafeToForeign _UnsafeBackdrop
                  , children: _
                  , className: classes.modal
                  , closeAfterTransition: true
                  , open
                  , onClose: handleClose
                  }
                $ fade
                $ { in: open
                  , children: _
                  , timeout: TransitionGroup.single 1000.0
                  }
                $ DOM.div
                $ { className: classes.paper, children: _ }
                $ [ DOM.h2 { id: "transition-modal-title", children: [ DOM.text "Transition modal" ] }
                  , DOM.p { id: "transition-modal-description", children: [ DOM.text "react-transition-group animates me" ] }
                  ]
            ]

type Components
  = { drawerList :: ReactComponent {}
    , transitionsModal :: ReactComponent {}
    }

-- | XXX:
-- | This kind of unsafe execution should be
-- | only on the top level of the module.
-- | You SHOULD NOT add any constraint to the
-- | type here - like taking the props record
-- | and tranforming it to the final opaque value.
appBar' = unsafePerformEffect $ appBarWithStyles (\t → { root: style t })
  where
  style t =
    mediaQuery (t.breakpoints.down Styles.md)
      (jss { backgroundColor: t.palette.primary.dark })
      <> mediaQuery (t.breakpoints.up Styles.md)
          (jss { backgroundColor: t.palette.secondary.dark })

loginButton =
  unsafePerformEffect
    $ buttonWithStyles
        (\t -> { root: jss { marginRight: theme.spacing 2.0 } })

textInputStyle t = { root: jss { width: "80%", margin: theme.spacing 2.0 } }

spacedFormControl = unsafePerformEffect $ formControlWithStyles textInputStyle

spacedOutlinedTextField = unsafePerformEffect $ outlinedWithStyles textInputStyle

spacedStandardTextField = unsafePerformEffect $ standardWithStyles textInputStyle

spacedFilledTextField = unsafePerformEffect $ filledWithStyles textInputStyle

menuIcon = unsafePerformEffect $ iconWithStyles (\t -> { root: jss { marginRight: t.spacing 2.0 } }) menu

app :: Components -> JSX
app components = make component { initialState: {}, render } {}
  where
  render self =
    muiThemeProvider $ { theme, children: _ }
      $ fragment --DOM.div $ { children: _ }
          [ cssBaseline
          , appBar' $ AppBar.props $ { children: _, position: AppBar.position.static } <<< Array.singleton
              $ toolbar
              $ { children: _ }
              $ [ menuIcon $ Icons.props {}
                , typography $ { children: _, variant: Typography.variant.h6 } <<< Array.singleton
                    $ link { children: [ DOM.text "LINK" ], href: "#TEST", color: Link.color.inherit }
                , loginButton
                    $ Button.props
                        { children: [ DOM.text "Login" ]
                        , color: Button.color.inherit
                        }
                , hidden
                    { implementation: Hidden.implementation.css
                    , children: [ DOM.text "`Hidden` visible on sm, lg, xl " ]
                    , only: Hidden.only.only [ Hidden.only.xs, Hidden.only.md ]
                    }
                , box
                    $ { children:
                          [ DOM.text "`Box` visible on md and larger justify-content: flex-end" ]
                      , display: hiding { xs: Only.none, sm: Only.none, md: Only.flex }
                      , flexGrow: 1.0
                      , justifyContent: JustifyContent.flexEnd
                      }
                ]
          , container
              $ { fixed: true, children: _ }
                  [ DOM.form $ { children: _ }
                      $ [ grid
                            $ { container: true, children: _ }
                                [ gridItem
                                    $ spacedFormControl
                                    $ FormControl.props
                                    $ { children: _ }
                                        [ inputLabel
                                            { htmlFor: "m-input"
                                            , children: [ DOM.text "Email address" ]
                                            }
                                        , input { placeholder: "your email address" }
                                        , formHelperText { id: "my-helper-text", children: [ DOM.text "We'll never share your email" ] }
                                        ]
                                , gridItem $ spacedStandardTextField $ StandardTextField.props
                                    $ { error: true
                                      , helperText:
                                          DOM.span
                                            $ { children: _ }
                                                [ DOM.text "A link inside a helper text: "
                                                , DOM.a { href: "https://example.com", children: [ DOM.text "example.com" ] }
                                                ]
                                      -- { dangerouslySetInnerHTML: { __html : "<a href=\"https://google.com\">UNSAFE</a>" }}
                                      , label: inputLabel { children: [ DOM.text "Label" ] }
                                      , placeholder: "test"
                                      }
                                , gridItem $ element components.transitionsModal {}
                                , gridItem $ element components.drawerList {}
                                , gridItem $ spacedStandardTextField
                                    $ StandardTextField.props
                                        { error: false
                                        , label: inputLabel { children: [ DOM.text "Label" ] }
                                        , placeholder: "test"
                                        }
                                -- , gridItem $ spacedFilledTextField
                                --     $ FilledTextField.props
                                --         { error: true
                                --         , label: inputLabel { children: [ DOM.text "Label" ] }
                                --         , placeholder: "test"
                                --         }
                                ]
                        ]
                  , divider { variant: Dividier.variant.middle }
                  , grid
                      $ { container: true, children: _ }
                          [ gridItem
                              $ buttonGroup
                              $ { variant: ButtonGroup.variant.text, color: ButtonGroup.color.primary, children: _ }
                                  [ Button.button { children: [ DOM.text "One" ] }
                                  , Button.button { children: [ DOM.text "Two" ] }
                                  , Button.button { children: [ DOM.text "Three" ] }
                                  ]
                          , gridItem
                              $ badge
                                  { badgeContent: DOM.text "4"
                                  , children: [ DOM.text "Badge example" ]
                                  , color: Badge.color.secondary
                                  }
                          ]
                  ]
          ]

main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  components <- { drawerList: _, transitionsModal: _ } <$> drawerList <*> transitionsModal
  case container of
    Nothing -> throw "Container element not found."
    Just c -> render (app components) c
