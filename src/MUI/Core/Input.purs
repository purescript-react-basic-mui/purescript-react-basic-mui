module MUI.Core.Input where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
import MUI.Core.InputBase (InputBasePropsOptions) as MUI.Core.InputBase
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
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

type InputPropsOptions componentProps = ( autoComplete :: String, autoFocus :: Boolean, className :: String, classes :: InputClassKey, color :: Color, defaultValue :: Foreign.Foreign, disableUnderline :: Boolean, disabled :: Boolean, endAdornment :: React.Basic.JSX, error :: Boolean, fullWidth :: Boolean, id :: String, inputProps :: Foreign.Foreign, inputRef :: Foreign.Foreign, margin :: Margin, multiline :: Boolean, name :: String, onChange :: React.Basic.Events.EventHandler, placeholder :: String, readOnly :: Boolean, required :: Boolean, rows :: Rows, rowsMax :: RowsMax, startAdornment :: React.Basic.JSX, "type" :: String, value :: Foreign.Foreign | componentProps )

foreign import data InputProps :: Type

foreign import data InputPropsPartial :: Type

inputPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (InputPropsOptions (MUI.Core.InputBase.InputBasePropsOptions React.Basic.DOM.Props_div)) => Record options -> InputPropsPartial
inputPropsPartial = Unsafe.Coerce.unsafeCoerce

type InputClassKeyGenericOptions a = ( colorSecondary :: a, disabled :: a, error :: a, focused :: a, formControl :: a, fullWidth :: a, input :: a, inputMarginDense :: a, inputMultiline :: a, inputTypeSearch :: a, multiline :: a, root :: a, underline :: a )

type InputClassKeyOptions  = InputClassKeyGenericOptions String

foreign import data InputClassKey :: Type

inputClassKey :: ∀ required given. Prim.Row.Union given required InputClassKeyOptions => Record given -> InputClassKey
inputClassKey = Unsafe.Coerce.unsafeCoerce

type InputClassKeyOptionsJSS  = InputClassKeyGenericOptions MUI.Core.JSS

foreign import data InputClassKeyJSS :: Type

inputClassKeyJSS :: ∀ required given. Prim.Row.Union given required InputClassKeyOptionsJSS => Record given -> InputClassKeyJSS
inputClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Input :: ∀ a. React.Basic.ReactComponent a

input :: ∀ required given. Prim.Row.Union given required (InputPropsOptions (MUI.Core.InputBase.InputBasePropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
input = React.Basic.element _Input

input_component :: ∀ required given componentProps. Prim.Row.Union given required (InputPropsOptions componentProps) => Record given -> React.Basic.JSX
input_component = React.Basic.element _Input

inputWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (InputPropsOptions (MUI.Core.InputBase.InputBasePropsOptions React.Basic.DOM.Props_div)) => Prim.Row.Union jss jss_ InputClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
inputWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Input)