module MUI.Core.Badge where

import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, Props_div, ReactComponent) as React.Basic
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

foreign import data Variant :: Type

variant :: { dot ∷ Variant, standard ∷ Variant }
variant = { dot: Unsafe.Coerce.unsafeCoerce "dot", standard: Unsafe.Coerce.unsafeCoerce "standard" }

foreign import data Color :: Type

color :: { default ∷ Color, error ∷ Color, primary ∷ Color, secondary ∷ Color }
color = { default: Unsafe.Coerce.unsafeCoerce "default", error: Unsafe.Coerce.unsafeCoerce "error", primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

foreign import data Vertical :: Type

vertical :: { bottom ∷ Vertical, top ∷ Vertical }
vertical = { bottom: Unsafe.Coerce.unsafeCoerce "bottom", top: Unsafe.Coerce.unsafeCoerce "top" }

foreign import data Horizontal :: Type

horizontal :: { left ∷ Horizontal, right ∷ Horizontal }
horizontal = { left: Unsafe.Coerce.unsafeCoerce "left", right: Unsafe.Coerce.unsafeCoerce "right" }

type PropsOptions componentProps = { anchorOrigin ∷ { horizontal ∷ Horizontal, vertical ∷ Vertical }, badgeContent ∷ React.Basic.JSX, children ∷ Array (React.Basic.JSX), classes ∷ ClassKey, color ∷ Color, component ∷ React.Basic.ReactComponent {  | componentProps }, invisible ∷ Boolean, max ∷ Number, showZero ∷ Boolean, variant ∷ Variant | componentProps }

type ClassKeyGenericOptions a = ( anchorOriginBottomLeftRectangle ∷ a, anchorOriginBottomRightCircle ∷ a, anchorOriginBottomRightRectangle ∷ a, anchorOriginTopLeftCircle ∷ a, anchorOriginTopLeftRectangle ∷ a, anchorOriginTopRightCircle ∷ a, anchorOriginTopRightRectangle ∷ a, badge ∷ a, colorError ∷ a, colorPrimary ∷ a, colorSecondary ∷ a, dot ∷ a, invisible ∷ a, root ∷ a )

type ClassKeyOptions  = ClassKeyGenericOptions String

foreign import data ClassKey :: Type

classKey :: ∀ required given. Prim.Row.Union given required ClassKeyOptions ⇒ Record given → ClassKey
classKey = Unsafe.Coerce.unsafeCoerce

type ClassKeyOptionsJSS  = ClassKeyGenericOptions MUI.Core.JSS

foreign import data ClassKeyJSS :: Type

classKeyJSS :: ∀ required given. Prim.Row.Union given required ClassKeyOptionsJSS ⇒ Record given → ClassKeyJSS
classKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Component :: ∀ a. React.Basic.ReactComponent a

component :: ∀ required given. Prim.Row.Union given required (PropsOptions React.Basic.Props_div) ⇒ Record given → ClassKeyJSS
component = React.Basic.element _Component

component :: ∀ required given componentProps. Prim.Row.Union given required (PropsOptions componentProps) ⇒ Record given → ClassKeyJSS
component = React.Basic.element _Component
