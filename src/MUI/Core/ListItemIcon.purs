module MUI.Core.ListItemIcon where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type ListItemIconPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: ListItemIconClassKey | componentProps )

foreign import data ListItemIconProps :: Type

foreign import data ListItemIconPropsPartial :: Type

listItemIconPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (ListItemIconPropsOptions React.Basic.DOM.Props_div) => Record options -> ListItemIconPropsPartial
listItemIconPropsPartial = Unsafe.Coerce.unsafeCoerce

type ListItemIconClassKeyGenericOptions a = ( root :: a )

type ListItemIconClassKeyOptions  = ListItemIconClassKeyGenericOptions String

foreign import data ListItemIconClassKey :: Type

listItemIconClassKey :: ∀ required given. Prim.Row.Union given required ListItemIconClassKeyOptions => Record given -> ListItemIconClassKey
listItemIconClassKey = Unsafe.Coerce.unsafeCoerce

type ListItemIconClassKeyOptionsJSS  = ListItemIconClassKeyGenericOptions MUI.Core.JSS

foreign import data ListItemIconClassKeyJSS :: Type

listItemIconClassKeyJSS :: ∀ required given. Prim.Row.Union given required ListItemIconClassKeyOptionsJSS => Record given -> ListItemIconClassKeyJSS
listItemIconClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _ListItemIcon :: ∀ a. React.Basic.ReactComponent a

listItemIcon :: ∀ required given. Prim.Row.Union given required (ListItemIconPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
listItemIcon = React.Basic.element _ListItemIcon

listItemIcon_component :: ∀ required given componentProps. Prim.Row.Union given required (ListItemIconPropsOptions componentProps) => Record given -> React.Basic.JSX
listItemIcon_component = React.Basic.element _ListItemIcon

listItemIconWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (ListItemIconPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ ListItemIconClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
listItemIconWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _ListItemIcon)