module MUI.Core.MenuItem where

import MUI.Core (JSS) as MUI.Core
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type MenuItemPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: MenuItemClassKey, dense :: Boolean, disableGutters :: Boolean | componentProps )

foreign import data MenuItemProps :: Type

type MenuItemClassKeyGenericOptions a = ( dense :: a, gutters :: a, root :: a, selected :: a )

type MenuItemClassKeyOptions  = MenuItemClassKeyGenericOptions String

foreign import data MenuItemClassKey :: Type

menuItemClassKey :: ∀ required given. Prim.Row.Union given required MenuItemClassKeyOptions => Record given -> MenuItemClassKey
menuItemClassKey = Unsafe.Coerce.unsafeCoerce

type MenuItemClassKeyOptionsJSS  = MenuItemClassKeyGenericOptions MUI.Core.JSS

foreign import data MenuItemClassKeyJSS :: Type

menuItemClassKeyJSS :: ∀ required given. Prim.Row.Union given required MenuItemClassKeyOptionsJSS => Record given -> MenuItemClassKeyJSS
menuItemClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _MenuItem :: ∀ a. React.Basic.ReactComponent a

menuItem :: ∀ required given. Prim.Row.Union given required (MenuItemPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
menuItem = React.Basic.element _MenuItem

menuItem_component :: ∀ required given componentProps. Prim.Row.Union given required (MenuItemPropsOptions componentProps) => Record given -> React.Basic.JSX
menuItem_component = React.Basic.element _MenuItem