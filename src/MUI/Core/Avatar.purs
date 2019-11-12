module MUI.Core.Avatar where

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

variant :: { circle :: Variant, rounded :: Variant, square :: Variant }
variant = { circle: Unsafe.Coerce.unsafeCoerce "circle", rounded: Unsafe.Coerce.unsafeCoerce "rounded", square: Unsafe.Coerce.unsafeCoerce "square" }

instance eqVariant :: Eq Variant where
  eq = Unsafe.Reference.unsafeRefEq

type AvatarPropsOptions componentProps = ( alt :: String, classes :: AvatarClassKey, sizes :: String, src :: String, srcSet :: String, variant :: Variant | componentProps )

foreign import data AvatarProps :: Type

foreign import data AvatarPropsPartial :: Type

avatarPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (AvatarPropsOptions React.Basic.DOM.Props_div) => Record options -> AvatarPropsPartial
avatarPropsPartial = Unsafe.Coerce.unsafeCoerce

type AvatarClassKeyGenericOptions a = ( circle :: a, colorDefault :: a, img :: a, root :: a, rounded :: a, square :: a )

type AvatarClassKeyOptions  = AvatarClassKeyGenericOptions String

foreign import data AvatarClassKey :: Type

avatarClassKey :: ∀ required given. Prim.Row.Union given required AvatarClassKeyOptions => Record given -> AvatarClassKey
avatarClassKey = Unsafe.Coerce.unsafeCoerce

type AvatarClassKeyOptionsJSS  = AvatarClassKeyGenericOptions MUI.Core.JSS

foreign import data AvatarClassKeyJSS :: Type

avatarClassKeyJSS :: ∀ required given. Prim.Row.Union given required AvatarClassKeyOptionsJSS => Record given -> AvatarClassKeyJSS
avatarClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _Avatar :: ∀ a. React.Basic.ReactComponent a

avatar :: ∀ required given. Prim.Row.Union given required (AvatarPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
avatar = React.Basic.element _Avatar

avatar_component :: ∀ required given componentProps. Prim.Row.Union given required (AvatarPropsOptions componentProps) => Record given -> React.Basic.JSX
avatar_component = React.Basic.element _Avatar

avatarWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (AvatarPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ AvatarClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
avatarWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _Avatar)