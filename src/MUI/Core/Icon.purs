module MUI.Core.Icon where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_span) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data FontSize :: Type

fontSize :: { default :: FontSize, inherit :: FontSize, large :: FontSize, small :: FontSize }
fontSize = { default: Unsafe.Coerce.unsafeCoerce "default", inherit: Unsafe.Coerce.unsafeCoerce "inherit", large: Unsafe.Coerce.unsafeCoerce "large", small: Unsafe.Coerce.unsafeCoerce "small" }

foreign import data Color :: Type

color :: { action :: Color, default :: Color, disabled :: Color, error :: Color, inherit :: Color, primary :: Color, secondary :: Color }
color = { action: Unsafe.Coerce.unsafeCoerce "action", default: Unsafe.Coerce.unsafeCoerce "default", disabled: Unsafe.Coerce.unsafeCoerce "disabled", error: Unsafe.Coerce.unsafeCoerce "error", inherit: Unsafe.Coerce.unsafeCoerce "inherit", primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqFontSize :: Eq FontSize where
  eq = Unsafe.Reference.unsafeRefEq

type IconPropsOptions componentProps = ( classes :: IconClassKey, color :: Color, component :: React.Basic.ReactComponent {  | componentProps }, fontSize :: FontSize | componentProps )

foreign import data IconProps :: Type

foreign import data IconPropsPartial :: Type

iconPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (IconPropsOptions React.Basic.DOM.Props_span) => Record options -> IconPropsPartial
iconPropsPartial = Unsafe.Coerce.unsafeCoerce

type IconClassKeyGenericOptions a = ( colorAction :: a, colorDisabled :: a, colorError :: a, colorPrimary :: a, colorSecondary :: a, fontSizeInherit :: a, fontSizeLarge :: a, fontSizeSmall :: a, root :: a )

type IconClassKeyOptions  = IconClassKeyGenericOptions String

foreign import data IconClassKey :: Type

iconClassKey :: ∀ required given. Prim.Row.Union given required IconClassKeyOptions => Record given -> IconClassKey
iconClassKey = Unsafe.Coerce.unsafeCoerce

type IconClassKeyOptionsJSS  = IconClassKeyGenericOptions MUI.Core.JSS

foreign import data IconClassKeyJSS :: Type

iconClassKeyJSS :: ∀ required given. Prim.Row.Union given required IconClassKeyOptionsJSS => Record given -> IconClassKeyJSS
iconClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Icon :: ∀ a. React.Basic.ReactComponent a

icon :: ∀ required given. Prim.Row.Union given required (IconPropsOptions React.Basic.DOM.Props_span) => Record given -> React.Basic.JSX
icon = React.Basic.element _Icon

icon_component :: ∀ required given componentProps. Prim.Row.Union given required (IconPropsOptions componentProps) => Record given -> React.Basic.JSX
icon_component = React.Basic.element _Icon

iconWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (IconPropsOptions React.Basic.DOM.Props_span) => Prim.Row.Union jss jss_ IconClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
iconWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Icon)