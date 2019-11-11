module MUI.Core.Checkbox where

import MUI.Core (JSS) as MUI.Core
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_input) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Color :: Type

color :: { default :: Color, primary :: Color, secondary :: Color }
color = { default: Unsafe.Coerce.unsafeCoerce "default", primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

type CheckboxPropsOptions componentProps = ( checked :: Boolean, checkedIcon :: React.Basic.JSX, children :: Array React.Basic.JSX, classes :: CheckboxClassKey, color :: Color, disableRipple :: Boolean, disabled :: Boolean, icon :: React.Basic.JSX, id :: String, indeterminate :: Boolean, indeterminateIcon :: React.Basic.JSX | componentProps )

foreign import data CheckboxProps :: Type

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

checkbox :: ∀ required given. Prim.Row.Union given required (CheckboxPropsOptions React.Basic.DOM.Props_input) => Record given -> React.Basic.JSX
checkbox = React.Basic.element _Checkbox

checkbox_component :: ∀ required given componentProps. Prim.Row.Union given required (CheckboxPropsOptions componentProps) => Record given -> React.Basic.JSX
checkbox_component = React.Basic.element _Checkbox