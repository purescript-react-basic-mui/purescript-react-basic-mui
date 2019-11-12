module MUI.Core.Fab where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.ButtonBase (ButtonBasePropsOptions) as MUI.Core.ButtonBase
import Prelude
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_button) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce
import Unsafe.Reference (unsafeRefEq) as Unsafe.Reference

foreign import data Variant :: Type

variant :: { extended :: Variant, round :: Variant }
variant = { extended: Unsafe.Coerce.unsafeCoerce "extended", round: Unsafe.Coerce.unsafeCoerce "round" }

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

type FabPropsOptions componentProps = ( classes :: FabClassKey, color :: Color, disableFocusRipple :: Boolean, disabled :: Boolean, href :: String, size :: Size, variant :: Variant | componentProps )

foreign import data FabProps :: Type

foreign import data FabPropsPartial :: Type

type FabClassKeyGenericOptions a = ( colorInherit :: a, disabled :: a, extended :: a, focusVisible :: a, label :: a, primary :: a, root :: a, secondary :: a, sizeMedium :: a, sizeSmall :: a )

type FabClassKeyOptions  = FabClassKeyGenericOptions String

foreign import data FabClassKey :: Type

fabClassKey :: ∀ required given. Prim.Row.Union given required FabClassKeyOptions => Record given -> FabClassKey
fabClassKey = Unsafe.Coerce.unsafeCoerce

type FabClassKeyOptionsJSS  = FabClassKeyGenericOptions MUI.Core.JSS

foreign import data FabClassKeyJSS :: Type

fabClassKeyJSS :: ∀ required given. Prim.Row.Union given required FabClassKeyOptionsJSS => Record given -> FabClassKeyJSS
fabClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Fab :: ∀ a. React.Basic.ReactComponent a

fab :: ∀ required given. Prim.Row.Union given required (FabPropsOptions (MUI.Core.ButtonBase.ButtonBasePropsOptions React.Basic.DOM.Props_button)) => Record given -> React.Basic.JSX
fab = React.Basic.element _Fab

fab_component :: ∀ required given componentProps. Prim.Row.Union given required (FabPropsOptions componentProps) => Record given -> React.Basic.JSX
fab_component = React.Basic.element _Fab