module Examples.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Undefined.NoProblem (opt)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)
import Foreign (unsafeToForeign)
import MUI.Core (jss)
import MUI.Core.Badge (badge)
import MUI.Core.Badge (color) as Badge
import MUI.Core.FormControl (formControl)
import MUI.Core.FormHelperText (formHelperText)
import MUI.Core.Input (input)
import MUI.Core.InputLabel (inputLabel)
import MUI.Core.TextField (filled, outlined, standard) as TextField
import Prim.RowList (Cons, Nil) as RL
import Prim.RowList (class RowToList, kind RowList)
import React.Basic (Component, JSX, StateUpdate(..), createComponent, fragment, make, runUpdate)
import React.Basic.DOM (css, form, text) as DOM
import React.Basic.DOM (render)
import React.Basic.DOM as R
import React.Basic.DOM.Events (capture_)
import Type.Prelude (RLProxy(..))
import Type.Row (RProxy(..))
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.NonElementParentNode (getElementById)
import Web.HTML (window)
import Web.HTML.HTMLDocument (toNonElementParentNode)
import Web.HTML.Window (document)

component :: Component Props
component = createComponent "Counter"

type Props =
  { label :: String
  }

data Action
  = Increment



actions :: Props -> JSX
actions = make component { initialState, render }
  where
    initialState = { counter: 0 }
    update self = case _ of
      Increment ->
        UpdateAndSideEffects
          (self.state { counter = self.state.counter + 1 })
          \{ state } -> log $ "Count: " <> show state.counter

    send = runUpdate update
    -- let
    --   styles = withStyleClasses \theme → {
    --     root: jss {
    --       '& > *': {
    --         margin: theme.spacing(1),
    --         width: '25ch',
    --       }
    --     }
    --   }

    --   withStyleClasses styles 

    render self = DOM.form $ { style: DOM.css { "div": "margin: 20px" }, children: _ }
      [ badge { badgeContent: DOM.text "4", children: [ DOM.text "TEST" ], color: Badge.color.primary }
      , formControl $ { children: _ }
          [ inputLabel
            { htmlFor: "m-input"
            , children: [ DOM.text "Email address" ]
            }
          , input { placeholder: "your email address" }
          , formHelperText { id: "my-helper-text", children: [ DOM.text "We'll never share your email" ]}
          ]
      , TextField.outlined
          { error: true
          , label: inputLabel { children: [ DOM.text "Label" ]}
          , placeholder: "test"
          }
      , TextField.standard
          { error: false
          , label: inputLabel { children: [ DOM.text "Label" ]}
          , placeholder: "test"
          }
      , TextField.filled
          { error: true
          , label: inputLabel { children: [ DOM.text "Label" ]}
          , placeholder: "test"
          }
          -- <FormControl component="fieldset" className={classes.formControl}>
          --   <FormLabel component="legend">Assign responsibility</FormLabel>
          --   <FormGroup>
          --     <FormControlLabel
          --       control={<Checkbox checked={gilad} onChange={handleChange} name="gilad" />}
          --       label="Gilad Gray"
          --     />
          --     <FormControlLabel
          --       control={<Checkbox checked={jason} onChange={handleChange} name="jason" />}
          --       label="Jason Killian"
          --     />
          --     <FormControlLabel
          --       control={<Checkbox checked={antoine} onChange={handleChange} name="antoine" />}
          --       label="Antoine Llorca"
          --     />
          --   </FormGroup>
          --   <FormHelperText>Be careful</FormHelperText>
          -- </FormControl>
      --  MUI.appBar { children: [ DOM.text "TEST" ] }
      --,  MUI.button { children: [ DOM.text "TEST" ] }
      -- , R.button
      --   { onClick: capture_ $ send self Increment
      --   , children: [ R.text (self.props.label <> ": " <> show self.state.counter) ]
      --   }
      ]


main :: Effect Unit
main = do
  container <- getElementById "container" =<< (map toNonElementParentNode $ document =<< window)
  case container of
    Nothing -> throw "Container element not found."
    Just c  ->
      let app = actions { label: "Increment" }
       in render app c

