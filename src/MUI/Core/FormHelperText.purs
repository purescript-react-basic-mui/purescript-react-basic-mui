module MUI.Core.FormHelperText where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_p) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { filled :: Variant, outlined :: Variant, standard :: Variant }
variant = { filled: Unsafe.Coerce.unsafeCoerce "filled", outlined: Unsafe.Coerce.unsafeCoerce "outlined", standard: Unsafe.Coerce.unsafeCoerce "standard" }

foreign import data Margin :: Type

margin :: { dense :: Margin }
margin = { dense: Unsafe.Coerce.unsafeCoerce "dense" }

instance eqMargin :: Eq Margin where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type FormHelperTextPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: FormHelperTextClassKey, component :: React.Basic.ReactComponent {  | componentProps }, disabled :: Boolean, error :: Boolean, filled :: Boolean, focused :: Boolean, margin :: Margin, required :: Boolean, variant :: Variant | componentProps )

foreign import data FormHelperTextProps :: Type

foreign import data FormHelperTextPropsPartial :: Type

formHelperTextPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (FormHelperTextPropsOptions React.Basic.DOM.Props_p) => Record options -> FormHelperTextPropsPartial
formHelperTextPropsPartial = Unsafe.Coerce.unsafeCoerce

type FormHelperTextClassKeyGenericOptions a = ( contained :: a, disabled :: a, error :: a, filled :: a, focused :: a, marginDense :: a, required :: a, root :: a )

type FormHelperTextClassKeyOptions  = FormHelperTextClassKeyGenericOptions String

foreign import data FormHelperTextClassKey :: Type

formHelperTextClassKey :: ∀ required given. Prim.Row.Union given required FormHelperTextClassKeyOptions => Record given -> FormHelperTextClassKey
formHelperTextClassKey = Unsafe.Coerce.unsafeCoerce

type FormHelperTextClassKeyOptionsJSS  = FormHelperTextClassKeyGenericOptions MUI.Core.JSS

foreign import data FormHelperTextClassKeyJSS :: Type

formHelperTextClassKeyJSS :: ∀ required given. Prim.Row.Union given required FormHelperTextClassKeyOptionsJSS => Record given -> FormHelperTextClassKeyJSS
formHelperTextClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _FormHelperText :: ∀ a. React.Basic.ReactComponent a

formHelperText :: ∀ required given. Prim.Row.Union given required (FormHelperTextPropsOptions React.Basic.DOM.Props_p) => Record given -> React.Basic.JSX
formHelperText = React.Basic.element _FormHelperText

formHelperText_component :: ∀ required given componentProps. Prim.Row.Union given required (FormHelperTextPropsOptions componentProps) => Record given -> React.Basic.JSX
formHelperText_component = React.Basic.element _FormHelperText

formHelperTextWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (FormHelperTextPropsOptions React.Basic.DOM.Props_p) => Prim.Row.Union jss jss_ FormHelperTextClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
formHelperTextWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _FormHelperText)