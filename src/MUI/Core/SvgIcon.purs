module MUI.Core.SvgIcon where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM.SVG (Props_svg) as React.Basic.DOM.SVG
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data FontSize :: Type

fontSize :: { default :: FontSize, inherit :: FontSize, large :: FontSize, small :: FontSize }
fontSize = { default: Unsafe.Coerce.unsafeCoerce "default", inherit: Unsafe.Coerce.unsafeCoerce "inherit", large: Unsafe.Coerce.unsafeCoerce "large", small: Unsafe.Coerce.unsafeCoerce "small" }

foreign import data Color :: Type

color :: { action :: Color, disabled :: Color, error :: Color, inherit :: Color, primary :: Color, secondary :: Color }
color = { action: Unsafe.Coerce.unsafeCoerce "action", disabled: Unsafe.Coerce.unsafeCoerce "disabled", error: Unsafe.Coerce.unsafeCoerce "error", inherit: Unsafe.Coerce.unsafeCoerce "inherit", primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqFontSize :: Eq FontSize where
  eq = Unsafe.Reference.unsafeRefEq

type SvgIconPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: SvgIconClassKey, color :: Color, fontSize :: FontSize, htmlColor :: String, shapeRendering :: String, titleAccess :: String, viewBox :: String | componentProps )

foreign import data SvgIconProps :: Type

foreign import data SvgIconPropsPartial :: Type

svgIconPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (SvgIconPropsOptions React.Basic.DOM.SVG.Props_svg) => Record options -> SvgIconPropsPartial
svgIconPropsPartial = Unsafe.Coerce.unsafeCoerce

type SvgIconClassKeyGenericOptions a = ( colorAction :: a, colorDisabled :: a, colorError :: a, colorPrimary :: a, colorSecondary :: a, fontSizeInherit :: a, fontSizeLarge :: a, fontSizeSmall :: a, root :: a )

type SvgIconClassKeyOptions  = SvgIconClassKeyGenericOptions String

foreign import data SvgIconClassKey :: Type

svgIconClassKey :: ∀ required given. Prim.Row.Union given required SvgIconClassKeyOptions => Record given -> SvgIconClassKey
svgIconClassKey = Unsafe.Coerce.unsafeCoerce

type SvgIconClassKeyOptionsJSS  = SvgIconClassKeyGenericOptions MUI.Core.JSS

foreign import data SvgIconClassKeyJSS :: Type

svgIconClassKeyJSS :: ∀ required given. Prim.Row.Union given required SvgIconClassKeyOptionsJSS => Record given -> SvgIconClassKeyJSS
svgIconClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _SvgIcon :: ∀ a. React.Basic.ReactComponent a

svgIcon :: ∀ required given. Prim.Row.Union given required (SvgIconPropsOptions React.Basic.DOM.SVG.Props_svg) => Record given -> React.Basic.JSX
svgIcon = React.Basic.element _SvgIcon

svgIcon_component :: ∀ required given componentProps. Prim.Row.Union given required (SvgIconPropsOptions componentProps) => Record given -> React.Basic.JSX
svgIcon_component = React.Basic.element _SvgIcon

svgIconWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (SvgIconPropsOptions React.Basic.DOM.SVG.Props_svg) => Prim.Row.Union jss jss_ SvgIconClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
svgIconWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _SvgIcon)