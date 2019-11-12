module MUI.Core.Badge where

import MUI.Core (JSS) as MUI.Core
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { dot :: Variant, standard :: Variant }
variant = { dot: Unsafe.Coerce.unsafeCoerce "dot", standard: Unsafe.Coerce.unsafeCoerce "standard" }

foreign import data Color :: Type

color :: { default :: Color, error :: Color, primary :: Color, secondary :: Color }
color = { default: Unsafe.Coerce.unsafeCoerce "default", error: Unsafe.Coerce.unsafeCoerce "error", primary: Unsafe.Coerce.unsafeCoerce "primary", secondary: Unsafe.Coerce.unsafeCoerce "secondary" }

foreign import data Vertical :: Type

vertical :: { bottom :: Vertical, top :: Vertical }
vertical = { bottom: Unsafe.Coerce.unsafeCoerce "bottom", top: Unsafe.Coerce.unsafeCoerce "top" }

foreign import data Horizontal :: Type

horizontal :: { left :: Horizontal, right :: Horizontal }
horizontal = { left: Unsafe.Coerce.unsafeCoerce "left", right: Unsafe.Coerce.unsafeCoerce "right" }

instance eqHorizontal :: Eq Horizontal where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVertical :: Eq Vertical where
  eq = Unsafe.Reference.unsafeRefEq

instance eqColor :: Eq Color where
  eq = Unsafe.Reference.unsafeRefEq

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type BadgePropsOptions componentProps = ( anchorOrigin :: { horizontal :: Horizontal, vertical :: Vertical }, badgeContent :: React.Basic.JSX, children :: Array React.Basic.JSX, classes :: BadgeClassKey, color :: Color, component :: React.Basic.ReactComponent {  | componentProps }, invisible :: Boolean, max :: Number, showZero :: Boolean, variant :: Variant | componentProps )

foreign import data BadgeProps :: Type

foreign import data BadgePropsPartial :: Type

type BadgeClassKeyGenericOptions a = ( anchorOriginBottomLeftRectangle :: a, anchorOriginBottomRightCircle :: a, anchorOriginBottomRightRectangle :: a, anchorOriginTopLeftCircle :: a, anchorOriginTopLeftRectangle :: a, anchorOriginTopRightCircle :: a, anchorOriginTopRightRectangle :: a, badge :: a, colorError :: a, colorPrimary :: a, colorSecondary :: a, dot :: a, invisible :: a, root :: a )

type BadgeClassKeyOptions  = BadgeClassKeyGenericOptions String

foreign import data BadgeClassKey :: Type

badgeClassKey :: ∀ required given. Prim.Row.Union given required BadgeClassKeyOptions => Record given -> BadgeClassKey
badgeClassKey = Unsafe.Coerce.unsafeCoerce

type BadgeClassKeyOptionsJSS  = BadgeClassKeyGenericOptions MUI.Core.JSS

foreign import data BadgeClassKeyJSS :: Type

badgeClassKeyJSS :: ∀ required given. Prim.Row.Union given required BadgeClassKeyOptionsJSS => Record given -> BadgeClassKeyJSS
badgeClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Badge :: ∀ a. React.Basic.ReactComponent a

badge :: ∀ required given. Prim.Row.Union given required (BadgePropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
badge = React.Basic.element _Badge

badge_component :: ∀ required given componentProps. Prim.Row.Union given required (BadgePropsOptions componentProps) => Record given -> React.Basic.JSX
badge_component = React.Basic.element _Badge