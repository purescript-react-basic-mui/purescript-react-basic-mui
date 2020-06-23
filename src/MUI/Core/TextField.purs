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

import MUI.Core (class Nub')
import MUI.Core.FormControl (FormControlPropsRow, FormControlReqPropsRow)
import MUI.Core.TextField.FilledTextField (FilledTextFieldReqPropsRow, FilledTextFieldPropsRow)
import MUI.Core.TextField.FilledTextField (Variant, filledTextField, variant) as FilledTextField
import MUI.Core.TextField.OutlinedTextField (OutlinedTextFieldPropsRow, OutlinedTextFieldReqPropsRow)
import MUI.Core.TextField.OutlinedTextField (Variant, outlinedTextField, variant) as OutlinedTextField
import MUI.Core.TextField.StandardTextField (StandardTextFieldReqPropsRow, StandardTextFieldPropsRow)
import MUI.Core.TextField.StandardTextField (Variant, standardTextField, variant) as StandardTextField
import Prim.Row (class Cons, class Lacks) as Row
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX)
import React.Basic.DOM (Props_div)
import Record (insert) as Record
import Type.Equality (from)
import Type.Prelude (class TypeEquals, SProxy(..))

_variant = SProxy ∷ SProxy "variant"

standard ∷ ∀ given given_ optionalGiven optionalMissing props required. 
  Nub' (StandardTextFieldReqPropsRow (FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (StandardTextFieldPropsRow (FormControlPropsRow Props_div)) props =>
  TypeEquals { | given } { variant ∷ StandardTextField.Variant | given_ } ⇒
  Row.Lacks "variant" given_ ⇒
  Row.Cons "variant" StandardTextField.Variant given_ given ⇒
  Prim.Row.Union given optionalMissing props =>
  {   | given_  }  ->  JSX
standard given =
  let
    given' = from (Record.insert _variant StandardTextField.variant.standard given)
  in
    StandardTextField.standardTextField (given' ∷ { | given })

outlined ∷ ∀ given given_ optionalGiven optionalMissing props required. 
  Nub' (OutlinedTextFieldReqPropsRow (FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (OutlinedTextFieldPropsRow (FormControlPropsRow Props_div)) props =>
  TypeEquals { | given } { variant ∷ OutlinedTextField.Variant | given_ } ⇒
  Row.Lacks "variant" given_ ⇒
  Row.Cons "variant" OutlinedTextField.Variant given_ given ⇒
  Prim.Row.Union given optionalMissing props =>
  {   | given_  }  ->  JSX
outlined given =
  let
    given' = from (Record.insert _variant OutlinedTextField.variant.outlined given)
  in
    OutlinedTextField.outlinedTextField (given' ∷ { | given })

filled ∷ ∀ given given_ optionalGiven optionalMissing props required. 
  Nub' (FilledTextFieldReqPropsRow (FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (FilledTextFieldPropsRow (FormControlPropsRow Props_div)) props =>
  TypeEquals { | given } { variant ∷ FilledTextField.Variant | given_ } ⇒
  Row.Lacks "variant" given_ ⇒
  Row.Cons "variant" FilledTextField.Variant given_ given ⇒
  Prim.Row.Union given optionalMissing props =>
  {   | given_  }  ->  JSX
filled given =
  let
    given' = from (Record.insert _variant FilledTextField.variant.filled given)
  in
    FilledTextField.filledTextField (given' ∷ { | given })

