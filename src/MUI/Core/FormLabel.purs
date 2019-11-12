module MUI.Core.FormLabel where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_label) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Color :: Type

color :: { primary :: Color, secondary :: Color }
color = { primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

type FormLabelPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: FormLabelClassKey, color :: Color, disabled :: Boolean, error :: Boolean, filled :: Boolean, focused :: Boolean, required :: Boolean | componentProps )

foreign import data FormLabelProps :: Type

foreign import data FormLabelPropsPartial :: Type

formLabelPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (FormLabelPropsOptions React.Basic.DOM.Props_label) => Record options -> FormLabelPropsPartial
formLabelPropsPartial = Unsafe.Coerce.unsafeCoerce

type FormLabelClassKeyGenericOptions a = ( asterisk :: a, colorSecondary :: a, disabled :: a, error :: a, filled :: a, focused :: a, required :: a, root :: a )

type FormLabelClassKeyOptions  = FormLabelClassKeyGenericOptions String

foreign import data FormLabelClassKey :: Type

formLabelClassKey :: ∀ required given. Prim.Row.Union given required FormLabelClassKeyOptions => Record given -> FormLabelClassKey
formLabelClassKey = Unsafe.Coerce.unsafeCoerce

type FormLabelClassKeyOptionsJSS  = FormLabelClassKeyGenericOptions MUI.Core.JSS

foreign import data FormLabelClassKeyJSS :: Type

formLabelClassKeyJSS :: ∀ required given. Prim.Row.Union given required FormLabelClassKeyOptionsJSS => Record given -> FormLabelClassKeyJSS
formLabelClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _FormLabel :: ∀ a. React.Basic.ReactComponent a

formLabel :: ∀ required given. Prim.Row.Union given required (FormLabelPropsOptions React.Basic.DOM.Props_label) => Record given -> React.Basic.JSX
formLabel = React.Basic.element _FormLabel

formLabel_component :: ∀ required given componentProps. Prim.Row.Union given required (FormLabelPropsOptions componentProps) => Record given -> React.Basic.JSX
formLabel_component = React.Basic.element _FormLabel

formLabelWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (FormLabelPropsOptions React.Basic.DOM.Props_label) => Prim.Row.Union jss jss_ FormLabelClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
formLabelWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _FormLabel)