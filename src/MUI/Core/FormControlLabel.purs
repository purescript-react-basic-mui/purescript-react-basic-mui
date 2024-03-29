{- This module was autogenerated. Please don't edit. -}
module MUI.Core.FormControlLabel where

import Effect (Effect) as Effect
import MUI.Core (JSS, class Nub')
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_label) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data LabelPlacement :: Type

labelPlacement ::
  { bottom :: LabelPlacement
  , end :: LabelPlacement
  , start :: LabelPlacement
  , top :: LabelPlacement
  }
labelPlacement = { bottom: unsafeCoerce "bottom", end: unsafeCoerce "end", start: unsafeCoerce "start", top: unsafeCoerce "top" }

instance eqLabelPlacement :: Eq LabelPlacement where
  eq = unsafeRefEq

type FormControlLabelClassesGenericRow a
  = ( disabled :: a
    , label :: a
    , labelPlacementBottom :: a
    , labelPlacementStart :: a
    , labelPlacementTop :: a
    , root :: a
    )

type FormControlLabelClassesKey
  = FormControlLabelClassesGenericRow String

type FormControlLabelClassesJSS
  = FormControlLabelClassesGenericRow JSS

type FormControlLabelOptPropsRow (r :: # Type)
  = ( checked :: Boolean
    , classes :: { | FormControlLabelClassesKey }
    , disabled :: Boolean
    , labelPlacement :: LabelPlacement
    , name :: String
    , onChange :: React.Basic.Events.EventHandler
    , value :: String
    | r
    )

type FormControlLabelReqPropsRow (r :: # Type)
  = ( control :: JSX
    , label :: JSX
    | r
    )

type FormControlLabelPropsRow (r :: # Type)
  = FormControlLabelOptPropsRow (FormControlLabelReqPropsRow r)

foreign import _UnsafeFormControlLabel :: forall componentProps. ReactComponent { | FormControlLabelPropsRow componentProps }

_FormControlLabel ::
  forall given optionalGiven optionalMissing props required.
  Nub' (FormControlLabelReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FormControlLabelPropsRow React.Basic.DOM.Props_label) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_FormControlLabel = unsafeCoerce _UnsafeFormControlLabel

formControlLabel ::
  forall given optionalGiven optionalMissing props required.
  Nub' (FormControlLabelReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FormControlLabelPropsRow React.Basic.DOM.Props_label) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
formControlLabel ps = element _FormControlLabel ps

formControlLabel' :: FormControlLabelProps -> JSX
formControlLabel' = MUI.React.Basic.element _FormControlLabel'

_FormControlLabel' :: ReactComponent FormControlLabelProps
_FormControlLabel' = unsafeCoerce _UnsafeFormControlLabel

formControlLabelWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ FormControlLabelClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (FormControlLabelProps -> JSX)
formControlLabelWithStyles style = render
  where
  withStyles' :: ReactComponent FormControlLabelProps -> Effect.Effect (ReactComponent FormControlLabelProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _FormControlLabel'

  render = map MUI.React.Basic.element styledComponent

foreign import data FormControlLabelProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (FormControlLabelReqPropsRow ()) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FormControlLabelPropsRow React.Basic.DOM.Props_label) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> FormControlLabelProps
props = unsafeCoerce
