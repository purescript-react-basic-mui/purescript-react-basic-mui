module MUI.Core.Toolbar where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { dense :: Variant, regular :: Variant }
variant = { dense: Unsafe.Coerce.unsafeCoerce "dense", regular: Unsafe.Coerce.unsafeCoerce "regular" }

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type ToolbarPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: ToolbarClassKey, disableGutters :: Boolean, variant :: Variant | componentProps )

foreign import data ToolbarProps :: Type

foreign import data ToolbarPropsPartial :: Type

toolbarPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ToolbarPropsOptions React.Basic.DOM.Props_div) => Record options -> ToolbarPropsPartial
toolbarPropsPartial = Unsafe.Coerce.unsafeCoerce

type ToolbarClassKeyGenericOptions a = ( dense :: a, gutters :: a, regular :: a, root :: a )

type ToolbarClassKeyOptions  = ToolbarClassKeyGenericOptions String

foreign import data ToolbarClassKey :: Type

toolbarClassKey :: ∀ required given. Prim.Row.Union given required ToolbarClassKeyOptions => Record given -> ToolbarClassKey
toolbarClassKey = Unsafe.Coerce.unsafeCoerce

type ToolbarClassKeyOptionsJSS  = ToolbarClassKeyGenericOptions MUI.Core.JSS

foreign import data ToolbarClassKeyJSS :: Type

toolbarClassKeyJSS :: ∀ required given. Prim.Row.Union given required ToolbarClassKeyOptionsJSS => Record given -> ToolbarClassKeyJSS
toolbarClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Toolbar :: ∀ a. React.Basic.ReactComponent a

toolbar :: ∀ required given. Prim.Row.Union given required (ToolbarPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
toolbar = React.Basic.element _Toolbar

toolbar_component :: ∀ required given componentProps. Prim.Row.Union given required (ToolbarPropsOptions componentProps) => Record given -> React.Basic.JSX
toolbar_component = React.Basic.element _Toolbar

toolbarWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (ToolbarPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ ToolbarClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
toolbarWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Toolbar)