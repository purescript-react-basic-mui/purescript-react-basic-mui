{- This module was autogenerated. Please don't edit. -}
module MUI.Core.InputLabel where

import Effect (Effect) as Effect
import MUI.Core (JSS, class Nub')
import MUI.Core.FormLabel (FormLabelPropsRow, FormLabelReqPropsRow) as MUI.Core.FormLabel
import MUI.Core.Styles (Theme, withStyles) as MUI.Core.Styles
import MUI.React.Basic (element) as MUI.React.Basic
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_label) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data Color :: Type

color ::
  { primary :: Color
  , secondary :: Color
  }
color = { primary: unsafeCoerce "primary", secondary: unsafeCoerce "secondary" }

foreign import data Margin :: Type

margin ::
  { dense :: Margin
  }
margin = { dense: unsafeCoerce "dense" }

foreign import data Variant :: Type

variant ::
  { filled :: Variant
  , outlined :: Variant
  , standard :: Variant
  }
variant = { filled: unsafeCoerce "filled", outlined: unsafeCoerce "outlined", standard: unsafeCoerce "standard" }

instance eqVariant :: Eq Variant where
  eq = unsafeRefEq

instance eqMargin :: Eq Margin where
  eq = unsafeRefEq

instance eqColor :: Eq Color where
  eq = unsafeRefEq

type InputLabelClassesGenericRow a
  = ( animated :: a
    , asterisk :: a
    , disabled :: a
    , error :: a
    , filled :: a
    , focused :: a
    , formControl :: a
    , marginDense :: a
    , outlined :: a
    , required :: a
    , root :: a
    , shrink :: a
    )

type InputLabelClassesKey
  = InputLabelClassesGenericRow String

type InputLabelClassesJSS
  = InputLabelClassesGenericRow JSS

type InputLabelOptPropsRow (r :: # Type)
  = ( children :: Array JSX
    , classes :: { | InputLabelClassesKey }
    , color :: Color
    , disableAnimation :: Boolean
    , disabled :: Boolean
    , error :: Boolean
    , focused :: Boolean
    , margin :: Margin
    , required :: Boolean
    , shrink :: Boolean
    , variant :: Variant
    | r
    )

type InputLabelReqPropsRow (r :: # Type)
  = r

type InputLabelPropsRow (r :: # Type)
  = InputLabelOptPropsRow (InputLabelReqPropsRow r)

foreign import _UnsafeInputLabel :: forall componentProps. ReactComponent { | InputLabelPropsRow componentProps }

_InputLabel ::
  forall given optionalGiven optionalMissing props required.
  Nub' (InputLabelReqPropsRow (MUI.Core.FormLabel.FormLabelReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (InputLabelPropsRow (MUI.Core.FormLabel.FormLabelPropsRow React.Basic.DOM.Props_label)) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent { | given }
_InputLabel = unsafeCoerce _UnsafeInputLabel

inputLabel ::
  forall given optionalGiven optionalMissing props required.
  Nub' (InputLabelReqPropsRow (MUI.Core.FormLabel.FormLabelReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (InputLabelPropsRow (MUI.Core.FormLabel.FormLabelPropsRow React.Basic.DOM.Props_label)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> JSX
inputLabel ps = element _InputLabel ps

inputLabel' :: InputLabelProps -> JSX
inputLabel' = MUI.React.Basic.element _InputLabel'

_InputLabel' :: ReactComponent InputLabelProps
_InputLabel' = unsafeCoerce _UnsafeInputLabel

inputLabelWithStyles ::
  forall jss_ jss.
  Prim.Row.Union jss jss_ InputLabelClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect.Effect (InputLabelProps -> JSX)
inputLabelWithStyles style = render
  where
  withStyles' :: ReactComponent InputLabelProps -> Effect.Effect (ReactComponent InputLabelProps)
  withStyles' = MUI.Core.Styles.withStyles (unsafeCoerce style)

  styledComponent = withStyles' _InputLabel'

  render = map MUI.React.Basic.element styledComponent

foreign import data InputLabelProps :: Type

props ::
  forall given optionalGiven optionalMissing props required.
  Nub' (InputLabelReqPropsRow (MUI.Core.FormLabel.FormLabelReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (InputLabelPropsRow (MUI.Core.FormLabel.FormLabelPropsRow React.Basic.DOM.Props_label)) props =>
  Prim.Row.Union given optionalMissing props =>
  { | given } -> InputLabelProps
props = unsafeCoerce
