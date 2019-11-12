module MUI.Core.BottomNavigation where

import MUI.Core (JSS) as MUI.Core
import MUI.Core.Styles.Types (Theme) as MUI.Core.Styles.Types
import MUI.Core.Styles.WithStyles (withStyles) as MUI.Core.Styles.WithStyles
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type BottomNavigationPropsOptions componentProps = ( children :: Array React.Basic.JSX, classes :: BottomNavigationClassKey, component :: React.Basic.ReactComponent {  | componentProps }, onChange :: React.Basic.Events.EventHandler, showLabels :: Boolean | componentProps )

foreign import data BottomNavigationProps :: Type

foreign import data BottomNavigationPropsPartial :: Type

bottomNavigationPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (BottomNavigationPropsOptions React.Basic.DOM.Props_div) => Record options -> BottomNavigationPropsPartial
bottomNavigationPropsPartial = Unsafe.Coerce.unsafeCoerce

type BottomNavigationClassKeyGenericOptions a = ( root :: a )

type BottomNavigationClassKeyOptions  = BottomNavigationClassKeyGenericOptions String

foreign import data BottomNavigationClassKey :: Type

bottomNavigationClassKey :: ∀ required given. Prim.Row.Union given required BottomNavigationClassKeyOptions => Record given -> BottomNavigationClassKey
bottomNavigationClassKey = Unsafe.Coerce.unsafeCoerce

type BottomNavigationClassKeyOptionsJSS  = BottomNavigationClassKeyGenericOptions MUI.Core.JSS

foreign import data BottomNavigationClassKeyJSS :: Type

bottomNavigationClassKeyJSS :: ∀ required given. Prim.Row.Union given required BottomNavigationClassKeyOptionsJSS => Record given -> BottomNavigationClassKeyJSS
bottomNavigationClassKeyJSS = Unsafe.Coerce.unsafeCoerce

foreign import _BottomNavigation :: ∀ a. React.Basic.ReactComponent a

bottomNavigation :: ∀ required given. Prim.Row.Union given required (BottomNavigationPropsOptions React.Basic.DOM.Props_div) => Record given -> React.Basic.JSX
bottomNavigation = React.Basic.element _BottomNavigation

bottomNavigation_component :: ∀ required given componentProps. Prim.Row.Union given required (BottomNavigationPropsOptions componentProps) => Record given -> React.Basic.JSX
bottomNavigation_component = React.Basic.element _BottomNavigation

bottomNavigationWithStyles :: ∀ required jss_ jss given. Prim.Row.Union given required (BottomNavigationPropsOptions React.Basic.DOM.Props_div) => Prim.Row.Union jss jss_ BottomNavigationClassKeyOptionsJSS => (MUI.Core.Styles.Types.Theme -> Record jss) -> Record given -> React.Basic.JSX
bottomNavigationWithStyles style = React.Basic.element (Unsafe.Coerce.unsafeCoerce MUI.Core.Styles.WithStyles.withStyles style _BottomNavigation)