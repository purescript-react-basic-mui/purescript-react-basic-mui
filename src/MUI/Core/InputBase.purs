module MUI.Core.InputBase where

import Foreign (Foreign) as Foreign
import MUI.Core (JSS) as MUI.Core
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

type InputBasePropsOptions componentProps = ( autoComplete :: String, autoFocus :: Boolean, className :: String, classes :: InputBaseClassKey, color :: Color, defaultValue :: Foreign.Foreign, disabled :: Boolean, endAdornment :: React.Basic.JSX, error :: Boolean, fullWidth :: Boolean, id :: String, inputProps :: Foreign.Foreign, inputRef :: Foreign.Foreign, margin :: Margin, multiline :: Boolean, name :: String, onChange :: React.Basic.Events.EventHandler, placeholder :: String, readOnly :: Boolean, required :: Boolean, rows :: Rows, rowsMax :: RowsMax, startAdornment :: React.Basic.JSX, "type" :: String, value :: Foreign.Foreign | componentProps )

foreign import data InputBaseProps :: Type

foreign import data InputBasePropsPartial :: Type

inputBasePropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (InputBasePropsOptions React.Basic.DOM.Props_div) => Record options -> InputBasePropsPartial
inputBasePropsPartial = Unsafe.Coerce.unsafeCoerce

type InputBaseClassKeyGenericOptions a = ( adornedEnd :: a, adornedStart :: a, colorSecondary :: a, disabled :: a, error :: a, focused :: a, formControl :: a, fullWidth :: a, input :: a, inputAdornedEnd :: a, inputAdornedStart :: a, inputHiddenLabel :: a, inputMarginDense :: a, inputMultiline :: a, inputTypeSearch :: a, marginDense :: a, multiline :: a, root :: a )

type InputBaseClassKeyOptions  = InputBaseClassKeyGenericOptions String

foreign import data InputBaseClassKey :: Type

inputBaseClassKey :: ∀ required given. Prim.Row.Union given required InputBaseClassKeyOptions => Record given -> InputBaseClassKey
inputBaseClassKey = Unsafe.Coerce.unsafeCoerce

type InputBaseClassKeyOptionsJSS  = InputBaseClassKeyGenericOptions MUI.Core.JSS

foreign import data InputBaseClassKeyJSS :: Type

inputBaseClassKeyJSS :: ∀ required given. Prim.Row.Union given required InputBaseClassKeyOptionsJSS => Record given -> InputBaseClassKeyJSS
inputBaseClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _InputBase :: ∀ a. React.Basic.ReactComponent a

inputBase :: ∀ required given. Prim.Row.Union given required (InputBasePropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
inputBase = React.Basic.element _InputBase

inputBase_component :: ∀ required given componentProps. Prim.Row.Union given required (InputBasePropsOptions componentProps) => Record given -> React.Basic.JSX
inputBase_component = React.Basic.element _InputBase

inputBaseWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (InputBasePropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ InputBaseClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
inputBaseWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _InputBase)