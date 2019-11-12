module MUI.Core.Divider where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_hr) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { fullWidth :: Variant, inset :: Variant, middle :: Variant }
variant = { fullWidth: Unsafe.Coerce.unsafeCoerce "fullWidth", inset: Unsafe.Coerce.unsafeCoerce "inset", middle: Unsafe.Coerce.unsafeCoerce "middle" }

foreign import data Orientation :: Type

orientation :: { horizontal :: Orientation, vertical :: Orientation }
orientation = { horizontal: Unsafe.Coerce.unsafeCoerce "horizontal", vertical: Unsafe.Coerce.unsafeCoerce "vertical" }

instance eqOrientation :: Eq Orientation where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type DividerPropsOptions componentProps = ( absolute :: Boolean, classes :: DividerClassKey, light :: Boolean, orientation :: Orientation, variant :: Variant | componentProps )

foreign import data DividerProps :: Type

foreign import data DividerPropsPartial :: Type

dividerPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (DividerPropsOptions React.Basic.DOM.Props_hr) => Record options -> DividerPropsPartial
dividerPropsPartial = Unsafe.Coerce.unsafeCoerce

type DividerClassKeyGenericOptions a = ( absolute :: a, inset :: a, light :: a, middle :: a, root :: a, vertical :: a )

type DividerClassKeyOptions  = DividerClassKeyGenericOptions String

foreign import data DividerClassKey :: Type

dividerClassKey :: ∀ required given. Prim.Row.Union given required DividerClassKeyOptions => Record given -> DividerClassKey
dividerClassKey = Unsafe.Coerce.unsafeCoerce

type DividerClassKeyOptionsJSS  = DividerClassKeyGenericOptions MUI.Core.JSS

foreign import data DividerClassKeyJSS :: Type

dividerClassKeyJSS :: ∀ required given. Prim.Row.Union given required DividerClassKeyOptionsJSS => Record given -> DividerClassKeyJSS
dividerClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Divider :: ∀ a. React.Basic.ReactComponent a

divider :: ∀ required given. Prim.Row.Union given required (DividerPropsOptions React.Basic.DOM.Props_hr) => Record given -> React.Basic.JSX
divider = React.Basic.element _Divider

divider_component :: ∀ required given componentProps. Prim.Row.Union given required (DividerPropsOptions componentProps) => Record given -> React.Basic.JSX
divider_component = React.Basic.element _Divider

dividerWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (DividerPropsOptions React.Basic.DOM.Props_hr) => Prim.Row.Union jss jss_ DividerClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
dividerWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Divider)