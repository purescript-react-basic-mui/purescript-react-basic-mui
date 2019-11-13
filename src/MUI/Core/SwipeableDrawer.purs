module MUI.Core.SwipeableDrawer where

import Foreign (Foreign) as Foreign
import MUI.Core.Drawer (DrawerPropsOptions) as MUI.Core.Drawer
import Prim.Row (class Union) as Prim.Row
import React.Basic (element, JSX, ReactComponent) as React.Basic
import React.Basic.DOM (Props_div) as React.Basic.DOM
import React.Basic.Events (EventHandler) as React.Basic.Events
import Unsafe.Coerce (unsafeCoerce) as Unsafe.Coerce

type SwipeableDrawerPropsOptions componentProps = ( "SwipeAreaProps" :: Foreign.Foreign, children :: Array React.Basic.JSX, disableBackdropTransition :: Boolean, disableDiscovery :: Boolean, disableSwipeToOpen :: Boolean, hysteresis :: Number, minFlingVelocity :: Number, onClose :: React.Basic.Events.EventHandler, onOpen :: React.Basic.Events.EventHandler, open :: Boolean, swipeAreaWidth :: Number | componentProps )

foreign import data SwipeableDrawerProps :: Type

foreign import data SwipeableDrawerPropsPartial :: Type

swipeableDrawerPropsPartial :: ∀ options_ options. Prim.Row.Union options options_ (SwipeableDrawerPropsOptions (MUI.Core.Drawer.DrawerPropsOptions React.Basic.DOM.Props_div)) => Record options -> SwipeableDrawerPropsPartial
swipeableDrawerPropsPartial = Unsafe.Coerce.unsafeCoerce

foreign import _SwipeableDrawer :: ∀ a. React.Basic.ReactComponent a

swipeableDrawer :: ∀ required given. Prim.Row.Union given required (SwipeableDrawerPropsOptions (MUI.Core.Drawer.DrawerPropsOptions React.Basic.DOM.Props_div)) => Record given -> React.Basic.JSX
swipeableDrawer = React.Basic.element _SwipeableDrawer

swipeableDrawer_component :: ∀ required given componentProps. Prim.Row.Union given required (SwipeableDrawerPropsOptions componentProps) => Record given -> React.Basic.JSX
swipeableDrawer_component = React.Basic.element _SwipeableDrawer