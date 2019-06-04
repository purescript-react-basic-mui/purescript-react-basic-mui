-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/SwipeableDrawer/SwipeableDrawer.d.ts
module MaterialUI.Basic.SwipeableDrawer where 
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)





foreign import _swipeableDrawer :: forall a. ReactComponent a



type SwipeableDrawerProps_optional  = 
  ( "SwipeAreaProps" :: (Object Foreign)
  ,  disableBackdropTransition :: Boolean
  ,  disableDiscovery :: Boolean
  ,  disableSwipeToOpen :: Boolean
  ,  hysteresis :: Number
  ,  minFlingVelocity :: Number
  ,  swipeAreaWidth :: Number
  ,  key :: String
  ,  children :: Array JSX
  )



type SwipeableDrawerProps_required   optional = 
  ( onClose :: EventHandler
  ,  onOpen :: EventHandler
  ,  open :: Boolean
  | optional
  )

swipeableDrawer
  :: forall attrs attrs_  
  . Union attrs attrs_ (SwipeableDrawerProps_optional  )
  => Record ((SwipeableDrawerProps_required  ) attrs)
  -> JSX
swipeableDrawer props = element _swipeableDrawer props  
