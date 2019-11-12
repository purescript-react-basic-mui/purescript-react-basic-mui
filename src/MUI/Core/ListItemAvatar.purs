module MUI.Core.ListItemAvatar where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type ListItemAvatarPropsOptions componentProps = ( classes :: ListItemAvatarClassKey | componentProps )

foreign import data ListItemAvatarProps :: Type

foreign import data ListItemAvatarPropsPartial :: Type

listItemAvatarPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ListItemAvatarPropsOptions React.Basic.DOM.Props_div) => Record options -> ListItemAvatarPropsPartial
listItemAvatarPropsPartial = Unsafe.Coerce.unsafeCoerce

type ListItemAvatarClassKeyGenericOptions a = ( icon :: a, root :: a )

type ListItemAvatarClassKeyOptions  = ListItemAvatarClassKeyGenericOptions String

foreign import data ListItemAvatarClassKey :: Type

listItemAvatarClassKey :: ∀ required given. Prim.Row.Union given required ListItemAvatarClassKeyOptions => Record given -> ListItemAvatarClassKey
listItemAvatarClassKey = Unsafe.Coerce.unsafeCoerce

type ListItemAvatarClassKeyOptionsJSS  = ListItemAvatarClassKeyGenericOptions MUI.Core.JSS

foreign import data ListItemAvatarClassKeyJSS :: Type

listItemAvatarClassKeyJSS :: ∀ required given. Prim.Row.Union given required ListItemAvatarClassKeyOptionsJSS => Record given -> ListItemAvatarClassKeyJSS
listItemAvatarClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _ListItemAvatar :: ∀ a. React.Basic.ReactComponent a

listItemAvatar :: ∀ required given. Prim.Row.Union given required (ListItemAvatarPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
listItemAvatar = React.Basic.element _ListItemAvatar

listItemAvatar_component :: ∀ required given componentProps. Prim.Row.Union given required (ListItemAvatarPropsOptions componentProps) => Record given -> React.Basic.JSX
listItemAvatar_component = React.Basic.element _ListItemAvatar

listItemAvatarWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (ListItemAvatarPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ ListItemAvatarClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
listItemAvatarWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _ListItemAvatar)