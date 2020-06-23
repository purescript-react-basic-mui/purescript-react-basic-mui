{- This module was autogenerated. Please don't edit. -}

module MUI.Core.TextField.StandardTextField where

import Data.Undefined.NoProblem (Opt)
import Data.Undefined.NoProblem.Mono (coerce, class Coerce)
import Foreign (Foreign) as Foreign
import MUI.Core (JSS, class Nub')
import MUI.Core.FormControl (FormControlPropsRow, FormControlReqPropsRow) as MUI.Core.FormControl
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (unsafeRefEq)

foreign import data Variant :: Type

variant::{ 
  standard :: Variant
 }
variant = { standard: unsafeCoerce "standard" }

foreign import data RowsMax :: Type

rowsMax::{ 
  number :: Number  ->  RowsMax,
  string :: String  ->  RowsMax
 }
rowsMax = { number: unsafeCoerce, string: unsafeCoerce }

foreign import data Rows :: Type

rows::{ 
  number :: Number  ->  Rows,
  string :: String  ->  Rows
 }
rows = { number: unsafeCoerce, string: unsafeCoerce }

foreign import data Margin :: Type

margin::{ 
  dense :: Margin,
  none :: Margin,
  normal :: Margin
 }
margin = { dense: unsafeCoerce "dense", none: unsafeCoerce "none", normal: unsafeCoerce "normal" }

foreign import data Color :: Type

color::{ 
  primary :: Color,
  secondary :: Color
 }
color = { primary: unsafeCoerce "primary", secondary: unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = unsafeRefEq

instance eqMargin :: Eq Margin where
  eq = unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = unsafeRefEq

type StandardTextFieldClassesGenericRow a =
  ( 
    root :: Opt a
   )

type StandardTextFieldClassesKey  =
  { 
   | StandardTextFieldClassesGenericRow String
   }

standardTextFieldClassesKey::forall given. 
  Coerce {   | given  } StandardTextFieldClassesKey =>
  {   | given  }  ->  StandardTextFieldClassesKey
standardTextFieldClassesKey = coerce

type StandardTextFieldClassesJSS  =
  { 
   | StandardTextFieldClassesGenericRow JSS
   }

standardTextFieldClassesJSS::forall given. 
  Coerce {   | given  } StandardTextFieldClassesJSS =>
  {   | given  }  ->  StandardTextFieldClassesJSS
standardTextFieldClassesJSS = coerce

type StandardTextFieldOptPropsRow (r :: # Type) =
  ( 
    autoComplete :: String,
    autoFocus :: Boolean,
    children :: Array  JSX,
    classes :: Opt StandardTextFieldClassesKey,
    color :: Color,
    defaultValue :: Foreign.Foreign,
    disabled :: Boolean,
    error :: Boolean,
    fullWidth :: Boolean,
    helperText :: JSX,
    id :: String,
    label :: JSX,
    margin :: Margin,
    multiline :: Boolean,
    name :: String,
    onBlur :: React.Basic.Events.EventHandler,
    onChange :: React.Basic.Events.EventHandler,
    onFocus :: React.Basic.Events.EventHandler,
    placeholder :: String,
    required :: Boolean,
    rows :: Rows,
    rowsMax :: RowsMax,
    select :: Boolean,
    "type" :: String,
    value :: Foreign.Foreign,
    variant :: Variant
   | r
   )

type StandardTextFieldReqPropsRow (r :: # Type) =
  r

type StandardTextFieldPropsRow (r :: # Type) =
  StandardTextFieldOptPropsRow (StandardTextFieldReqPropsRow r)

foreign import _UnsafeStandardTextField :: forall componentProps.    ReactComponent {   | StandardTextFieldPropsRow componentProps  }

_StandardTextField::forall given optionalGiven optionalMissing props required. 
  Nub' (StandardTextFieldReqPropsRow (MUI.Core.FormControl.FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (StandardTextFieldPropsRow (MUI.Core.FormControl.FormControlPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  ReactComponent {   | given  }
_StandardTextField = unsafeCoerce _UnsafeStandardTextField

standardTextField::forall given optionalGiven optionalMissing props required. 
  Nub' (StandardTextFieldReqPropsRow (MUI.Core.FormControl.FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (StandardTextFieldPropsRow (MUI.Core.FormControl.FormControlPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  {   | given  }  ->  JSX
standardTextField props = element _StandardTextField props

foreign import data StandardTextFieldProps :: Type

standardTextFieldProps::forall given optionalGiven optionalMissing props required. 
  Nub' (StandardTextFieldReqPropsRow (MUI.Core.FormControl.FormControlReqPropsRow ())) required =>
  Prim.Row.Union required optionalGiven given =>
  Nub' (StandardTextFieldPropsRow (MUI.Core.FormControl.FormControlPropsRow React.Basic.DOM.Props_div)) props =>
  Prim.Row.Union given optionalMissing props =>
  {   | StandardTextFieldReqPropsRow given  }  ->  StandardTextFieldProps
standardTextFieldProps = unsafeCoerce