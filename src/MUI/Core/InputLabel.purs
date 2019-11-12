module MUI.Core.InputLabel where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.FormLabel (FormLabelPropsOptions) as MUI.Core.FormLabel
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_label) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { filled :: Variant, outlined :: Variant, standard :: Variant }
variant = { filled: Unsafe.Coerce.unsafeCoerce "filled", outlined: Unsafe.Coerce.unsafeCoerce "outlined", standard: Unsafe.Coerce.unsafeCoerce "standard" }

foreign import data Margin :: Type

margin :: { dense :: Margin }
margin = { dense: Unsafe.Coerce.unsafeCoerce "dense" }

foreign import data Color :: Type

color :: { primary :: Color, secondary :: Color }
color = { primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqMargin :: Eq Margin where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type InputLabelPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: InputLabelClassKey, color :: Color, disableAnimation :: Boolean, disabled :: Boolean, error :: Boolean, focused :: Boolean, margin :: Margin, required :: Boolean, shrink :: Boolean, variant :: Variant | componentProps )

foreign import data InputLabelProps :: Type

foreign import data InputLabelPropsPartial :: Type

inputLabelPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (InputLabelPropsOptions (MUI.Core.FormLabel.FormLabelPropsOptions React.Basic.DOM.Props_label)) => Record options -> InputLabelPropsPartial
inputLabelPropsPartial = Unsafe.Coerce.unsafeCoerce

type InputLabelClassKeyGenericOptions a = ( animated :: a, asterisk :: a, disabled :: a, error :: a, filled :: a, focused :: a, formControl :: a, marginDense :: a, outlined :: a, required :: a, root :: a, shrink :: a )

type InputLabelClassKeyOptions  = InputLabelClassKeyGenericOptions String

foreign import data InputLabelClassKey :: Type

inputLabelClassKey :: ∀ required given. Prim.Row.Union given required InputLabelClassKeyOptions => Record given -> InputLabelClassKey
inputLabelClassKey = Unsafe.Coerce.unsafeCoerce

type InputLabelClassKeyOptionsJSS  = InputLabelClassKeyGenericOptions MUI.Core.JSS

foreign import data InputLabelClassKeyJSS :: Type

inputLabelClassKeyJSS :: ∀ required given. Prim.Row.Union given required InputLabelClassKeyOptionsJSS => Record given -> InputLabelClassKeyJSS
inputLabelClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _InputLabel :: ∀ a. React.Basic.ReactComponent a

inputLabel :: ∀ required given. Prim.Row.Union given required (InputLabelPropsOptions (MUI.Core.FormLabel.FormLabelPropsOptions React.Basic.DOM.Props_label)) => Record given -> React.Basic.JSX
inputLabel = React.Basic.element _InputLabel

inputLabel_component :: ∀ required given componentProps. Prim.Row.Union given required (InputLabelPropsOptions componentProps) => Record given -> React.Basic.JSX
inputLabel_component = React.Basic.element _InputLabel

inputLabelWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (InputLabelPropsOptions (MUI.Core.FormLabel.FormLabelPropsOptions React.Basic.DOM.Props_label)) => Prim.Row.Union jss jss_ InputLabelClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
inputLabelWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _InputLabel)