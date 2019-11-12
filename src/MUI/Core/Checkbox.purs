module MUI.Core.Checkbox where

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

foreign import data Type_ :: Type

type_ :: { button :: Type_, reset :: Type_, submit :: Type_ }
type_ = { button: Unsafe.Coerce.unsafeCoerce "button", reset: Unsafe.Coerce.unsafeCoerce "reset", submit: Unsafe.Coerce.unsafeCoerce "submit" }

foreign import data Color :: Type

color :: { default :: Color, primary :: Color, secondary :: Color }
color = { default: Unsafe.Coerce.unsafeCoerce "default", primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqType :: Eq Type_ where
  eq = Unsafe.Reference.unsafeRefEq

type CheckboxPropsOptions componentProps = ( checked :: Boolean, checkedIcon :: React.Basic.JSX, classes :: CheckboxClassKey, color :: Color, disableRipple :: Boolean, disabled :: Boolean, icon :: React.Basic.JSX, id :: String, indeterminate :: Boolean, indeterminateIcon :: React.Basic.JSX, inputProps :: Foreign.Foreign, inputRef :: Foreign.Foreign, onChange :: React.Basic.Events.EventHandler, required :: Boolean, "type" :: Type_, value :: Foreign.Foreign | componentProps )

foreign import data CheckboxProps :: Type

foreign import data CheckboxPropsPartial :: Type

checkboxPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (CheckboxPropsOptions React.Basic.DOM.Props_div) => Record options -> CheckboxPropsPartial
checkboxPropsPartial = Unsafe.Coerce.unsafeCoerce

type CheckboxClassKeyGenericOptions a = ( checked :: a, colorPrimary :: a, colorSecondary :: a, disabled :: a, indeterminate :: a, input :: a, root :: a )

type CheckboxClassKeyOptions  = CheckboxClassKeyGenericOptions String

foreign import data CheckboxClassKey :: Type

checkboxClassKey :: ∀ required given. Prim.Row.Union given required CheckboxClassKeyOptions => Record given -> CheckboxClassKey
checkboxClassKey = Unsafe.Coerce.unsafeCoerce

type CheckboxClassKeyOptionsJSS  = CheckboxClassKeyGenericOptions MUI.Core.JSS

foreign import data CheckboxClassKeyJSS :: Type

checkboxClassKeyJSS :: ∀ required given. Prim.Row.Union given required CheckboxClassKeyOptionsJSS => Record given -> CheckboxClassKeyJSS
checkboxClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Checkbox :: ∀ a. React.Basic.ReactComponent a

checkbox :: ∀ required given. Prim.Row.Union given required (CheckboxPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
checkbox = React.Basic.element _Checkbox

checkbox_component :: ∀ required given componentProps. Prim.Row.Union given required (CheckboxPropsOptions componentProps) => Record given -> React.Basic.JSX
checkbox_component = React.Basic.element _Checkbox

checkboxWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (CheckboxPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ CheckboxClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
checkboxWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Checkbox)