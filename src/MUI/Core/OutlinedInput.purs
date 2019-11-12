module MUI.Core.OutlinedInput where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import MUI.Core.InputBase (InputBasePropsOptions) as MUI.Core.InputBase
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data RowsMax :: Type

rowsMax :: { number :: Number -> RowsMax, string :: String -> RowsMax }
rowsMax = { number: Unsafe.Coerce.unsafeCoerce, string: Unsafe.Coerce.unsafeCoerce }

foreign import data Rows :: Type

rows :: { number :: Number -> Rows, string :: String -> Rows }
rows = { number: Unsafe.Coerce.unsafeCoerce, string: Unsafe.Coerce.unsafeCoerce }

foreign import data Margin :: Type

margin :: { dense :: Margin, none :: Margin }
margin = { dense: Unsafe.Coerce.unsafeCoerce "dense", none: Unsafe.Coerce.unsafeCoerce "none" }

foreign import data Color :: Type

color :: { primary :: Color, secondary :: Color }
color = { primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqMargin :: Eq Margin where
  eq = Unsafe.Reference.unsafeRefEq

type OutlinedInputPropsOptions componentProps = ( autoComplete :: String, autoFocus :: Boolean, className :: String, classes :: OutlinedInputClassKey, color :: Color, defaultValue :: Foreign.Foreign, disabled :: Boolean, endAdornment :: React.Basic.JSX, error :: Boolean, fullWidth :: Boolean, id :: String, inputProps :: Foreign.Foreign, inputRef :: Foreign.Foreign, margin :: Margin, multiline :: Boolean, name :: String, notched :: Boolean, onChange :: React.Basic.Events.EventHandler, placeholder :: String, readOnly :: Boolean, required :: Boolean, rows :: Rows, rowsMax :: RowsMax, startAdornment :: React.Basic.JSX, "type" :: String, value :: Foreign.Foreign | componentProps )

foreign import data OutlinedInputProps :: Type

foreign import data OutlinedInputPropsPartial :: Type

outlinedInputPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (OutlinedInputPropsOptions (MUI.Core.InputBase.InputBasePropsOptions React.Basic.DOM.Props_div)) => Record options -> OutlinedInputPropsPartial
outlinedInputPropsPartial = Unsafe.Coerce.unsafeCoerce

type OutlinedInputClassKeyGenericOptions a = ( adornedEnd :: a, adornedStart :: a, colorSecondary :: a, disabled :: a, error :: a, focused :: a, input :: a, inputAdornedEnd :: a, inputAdornedStart :: a, inputMarginDense :: a, inputMultiline :: a, inputSelect :: a, marginDense :: a, multiline :: a, notchedOutline :: a, root :: a )

type OutlinedInputClassKeyOptions  = OutlinedInputClassKeyGenericOptions String

foreign import data OutlinedInputClassKey :: Type

outlinedInputClassKey :: ∀ required given. Prim.Row.Union given required OutlinedInputClassKeyOptions => Record given -> OutlinedInputClassKey
outlinedInputClassKey = Unsafe.Coerce.unsafeCoerce

type OutlinedInputClassKeyOptionsJSS  = OutlinedInputClassKeyGenericOptions MUI.Core.JSS

foreign import data OutlinedInputClassKeyJSS :: Type

outlinedInputClassKeyJSS :: ∀ required given. Prim.Row.Union given required OutlinedInputClassKeyOptionsJSS => Record given -> OutlinedInputClassKeyJSS
outlinedInputClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _OutlinedInput :: ∀ a. React.Basic.ReactComponent a

outlinedInput :: ∀ required given. Prim.Row.Union given required (OutlinedInputPropsOptions (MUI.Core.InputBase.InputBasePropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
outlinedInput = React.Basic.element _OutlinedInput

outlinedInput_component :: ∀ required given componentProps. Prim.Row.Union given required (OutlinedInputPropsOptions componentProps) => Record given -> React.Basic.JSX
outlinedInput_component = React.Basic.element _OutlinedInput