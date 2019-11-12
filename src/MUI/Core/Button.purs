module MUI.Core.Button where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.ButtonBase (ButtonBasePropsOptions) as MUI.Core.ButtonBase
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_button) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { contained :: Variant, outlined :: Variant, text :: Variant }
variant = { contained: Unsafe.Coerce.unsafeCoerce "contained", outlined: Unsafe.Coerce.unsafeCoerce "outlined", text: Unsafe.Coerce.unsafeCoerce "text" }

foreign import data Size :: Type

size :: { large :: Size, medium :: Size, small :: Size }
size = { large: Unsafe.Coerce.unsafeCoerce "large", medium: Unsafe.Coerce.unsafeCoerce "medium", small: Unsafe.Coerce.unsafeCoerce "small" }

foreign import data Color :: Type

color :: { default :: Color, inherit :: Color, primary :: Color, secondary :: Color }
color = { default: Unsafe.Coerce.unsafeCoerce "default", inherit: Unsafe.Coerce.unsafeCoerce "inherit", primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqSize :: Eq Size where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type ButtonPropsOptions componentProps = ( classes :: ButtonClassKey, color :: Color, disableFocusRipple :: Boolean, disableRipple :: Boolean, disabled :: Boolean, endIcon :: React.Basic.JSX, fullWidth :: Boolean, href :: String, size :: Size, startIcon :: React.Basic.JSX, variant :: Variant | componentProps )

foreign import data ButtonProps :: Type

foreign import data ButtonPropsPartial :: Type

buttonPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ButtonPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record options -> ButtonPropsPartial
buttonPropsPartial = Unsafe.Coerce.unsafeCoerce

type ButtonClassKeyGenericOptions a = ( colorInherit :: a, contained :: a, containedPrimary :: a, containedSecondary :: a, containedSizeLarge :: a, containedSizeSmall :: a, disabled :: a, endIcon :: a, focusVisible :: a, fullWidth :: a, iconSizeLarge :: a, iconSizeMedium :: a, iconSizeSmall :: a, label :: a, outlined :: a, outlinedPrimary :: a, outlinedSecondary :: a, outlinedSizeLarge :: a, outlinedSizeSmall :: a, root :: a, sizeLarge :: a, sizeSmall :: a, startIcon :: a, text :: a, textPrimary :: a, textSecondary :: a, textSizeLarge :: a, textSizeSmall :: a )

type ButtonClassKeyOptions  = ButtonClassKeyGenericOptions String

foreign import data ButtonClassKey :: Type

buttonClassKey :: ∀ required given. Prim.Row.Union given required ButtonClassKeyOptions => Record given -> ButtonClassKey
buttonClassKey = Unsafe.Coerce.unsafeCoerce

type ButtonClassKeyOptionsJSS  = ButtonClassKeyGenericOptions MUI.Core.JSS

foreign import data ButtonClassKeyJSS :: Type

buttonClassKeyJSS :: ∀ required given. Prim.Row.Union given required ButtonClassKeyOptionsJSS => Record given -> ButtonClassKeyJSS
buttonClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Button :: ∀ a. React.Basic.ReactComponent a

button :: ∀ required given. Prim.Row.Union given required (ButtonPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record given -> React.Basic.JSX
button = React.Basic.element _Button

button_component :: ∀ required given componentProps. Prim.Row.Union given required (ButtonPropsOptions componentProps) => Record given -> React.Basic.JSX
button_component = React.Basic.element _Button

buttonWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (ButtonPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Prim.Row.Union jss jss_ ButtonClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
buttonWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Button)