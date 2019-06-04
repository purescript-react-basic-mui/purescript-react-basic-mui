-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Tooltip/Tooltip.d.ts
module MaterialUI.Basic.Tooltip where 
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (Component, JSX, ReactComponent, element)





foreign import _tooltip :: forall a. ReactComponent a



type TooltipProps_optional  = 
  ( "PopperProps" :: (Object Foreign)
  ,  "TransitionComponent" :: (Component Foreign)
  ,  "TransitionProps" :: Foreign
  ,  disableFocusListener :: Boolean
  ,  disableHoverListener :: Boolean
  ,  disableTouchListener :: Boolean
  ,  enterDelay :: Number
  ,  enterTouchDelay :: Number
  ,  id :: String
  ,  interactive :: Boolean
  ,  leaveDelay :: Number
  ,  leaveTouchDelay :: Number
  ,  onClose :: EventHandler
  ,  onOpen :: EventHandler
  ,  open :: Boolean
  ,  placement :: String
  ,  key :: String
  ,  children :: Array JSX
  )



type TooltipProps_required   optional = 
  ( title :: JSX
  | optional
  )

tooltip
  :: forall attrs attrs_  
  . Union attrs attrs_ (TooltipProps_optional  )
  => Record ((TooltipProps_required  ) attrs)
  -> JSX
tooltip props = element _tooltip props  
