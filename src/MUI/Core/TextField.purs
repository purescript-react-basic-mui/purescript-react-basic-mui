-- | This module was written by hand ;-)
-- |
-- | I'm not sure how to better encode this union
-- | on the PS side. On the TS side we have a union
-- | of props records which are dispatched on the
-- | `variant` property which in every case is a different
-- | literal:
-- |
-- | https://github.com/mui-org/material-ui/blob/3f9e9a50b4f36ab9b12baf7a16d9f35457702bae/packages/material-ui/src/TextField/TextField.d.ts#L209
-- |
-- | In some sens we are doing here exactly the same but
-- | the code is just a pile of constraints ;-)
-- |
module MUI.Core.TextField where

import Prelude
import Effect (Effect)
import MUI.Core (class Nub')
import MUI.Core.FormControl (FormControlPropsRow, FormControlReqPropsRow)
import MUI.Core.Styles (Theme) as MUI.Core.Styles
import MUI.Core.TextField.FilledTextField (FilledTextFieldClassesJSS, FilledTextFieldProps, FilledTextFieldPropsRow, FilledTextFieldReqPropsRow, filledTextFieldWithStyles)
import MUI.Core.TextField.FilledTextField (Variant, filledTextField, props, variant) as FilledTextField
import MUI.Core.TextField.OutlinedTextField (OutlinedTextFieldClassesJSS, OutlinedTextFieldProps, OutlinedTextFieldReqPropsRow, OutlinedTextFieldPropsRow, outlinedTextFieldWithStyles)
import MUI.Core.TextField.OutlinedTextField (Variant, outlinedTextField, props, variant) as OutlinedTextField
import MUI.Core.TextField.StandardTextField (StandardTextFieldClassesJSS, StandardTextFieldProps, StandardTextFieldReqPropsRow, StandardTextFieldPropsRow)
import MUI.Core.TextField.StandardTextField (Variant, props, standardTextField, standardTextFieldWithStyles, variant) as StandardTextField
import Prim.Row (class Cons, class Lacks) as Row
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX)
import React.Basic.DOM (Props_div)
import Record (insert) as Record
import Record.Unsafe (unsafeSet)
import Type.Equality (from)
import Type.Prelude (class TypeEquals, SProxy(..))
import Unsafe.Coerce (unsafeCoerce)

_variant = SProxy ∷ SProxy "variant"

standard ∷
  ∀ given given_ optionalGiven optionalMissing props required.
  Nub' (StandardTextFieldReqPropsRow (FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (StandardTextFieldPropsRow (FormControlPropsRow Props_div)) props =>
  TypeEquals { | given } { variant ∷ StandardTextField.Variant | given_ } ⇒
  Row.Lacks "variant" given_ ⇒
  Row.Cons "variant" StandardTextField.Variant given_ given ⇒
  Prim.Row.Union given optionalMissing props =>
  { | given_ } -> JSX
standard given =
  let
    given' = from (Record.insert _variant StandardTextField.variant.standard given)
  in
    StandardTextField.standardTextField (given' ∷ { | given })

standardWithStyles ∷
  ∀ jss jss_.
  Prim.Row.Union jss jss_ StandardTextFieldClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect (StandardTextFieldProps -> JSX)
standardWithStyles style = do
  render <- StandardTextField.standardTextFieldWithStyles style
  pure \props → do
    let
      propsRecord = unsafeCoerce props ∷ { | StandardTextFieldPropsRow () }

      propsRecord' = unsafeSet "variant" StandardTextField.variant.standard propsRecord ∷ { | StandardTextFieldPropsRow () }

      props' = StandardTextField.props propsRecord'
    render props

outlined ∷
  ∀ given given_ optionalGiven optionalMissing props required.
  Nub' (OutlinedTextFieldReqPropsRow (FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (OutlinedTextFieldPropsRow (FormControlPropsRow Props_div)) props =>
  TypeEquals { | given } { variant ∷ OutlinedTextField.Variant | given_ } ⇒
  Row.Lacks "variant" given_ ⇒
  Row.Cons "variant" OutlinedTextField.Variant given_ given ⇒
  Prim.Row.Union given optionalMissing props =>
  { | given_ } -> JSX
outlined given =
  let
    given' = from (Record.insert _variant OutlinedTextField.variant.outlined given)
  in
    OutlinedTextField.outlinedTextField (given' ∷ { | given })

outlinedWithStyles ∷
  ∀ jss jss_.
  Prim.Row.Union jss jss_ OutlinedTextFieldClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect (OutlinedTextFieldProps -> JSX)
outlinedWithStyles style = do
  render <- outlinedTextFieldWithStyles style
  pure \props → do
    let
      propsRecord = unsafeCoerce props ∷ { | OutlinedTextFieldPropsRow () }

      propsRecord' = unsafeSet "variant" OutlinedTextField.variant.outlined propsRecord ∷ { | OutlinedTextFieldPropsRow () }

      props' = OutlinedTextField.props propsRecord'
    render props

filled ∷
  ∀ given given_ optionalGiven optionalMissing props required.
  Nub' (FilledTextFieldReqPropsRow (FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FilledTextFieldPropsRow (FormControlPropsRow Props_div)) props =>
  TypeEquals { | given } { variant ∷ FilledTextField.Variant | given_ } ⇒
  Row.Lacks "variant" given_ ⇒
  Row.Cons "variant" FilledTextField.Variant given_ given ⇒
  Prim.Row.Union given optionalMissing props =>
  { | given_ } -> JSX
filled given =
  let
    given' = from (Record.insert _variant FilledTextField.variant.filled given)
  in
    FilledTextField.filledTextField (given' ∷ { | given })

filledWithStyles ∷
  ∀ jss jss_.
  Prim.Row.Union jss jss_ FilledTextFieldClassesJSS =>
  (MUI.Core.Styles.Theme -> { | jss }) -> Effect (FilledTextFieldProps -> JSX)
filledWithStyles style = do
  render <- filledTextFieldWithStyles style
  pure \props → do
    let
      propsRecord = unsafeCoerce props ∷ { | FilledTextFieldPropsRow () }

      propsRecord' = unsafeSet "variant" FilledTextField.variant.filled propsRecord ∷ { | FilledTextFieldPropsRow () }

      props' = FilledTextField.props propsRecord'
    render props
