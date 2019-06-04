-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Popover/Popover.d.ts
module MaterialUI.Basic.Popover where 
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (Component, JSX, ReactComponent, element)



foreign import data PopoverPropsAnchorEl :: Type
foreign import data PopoverPropsTransitionDuration :: Type

foreign import _popover :: forall a. ReactComponent a

type PopoverOrigin  = {
    horizontal :: String
  , vertical :: String
}

  
type PopoverPosition  = {
    left :: Number
  , top :: Number
}

  


type PopoverProps  = 
  ( "ModalClasses" :: (Object Foreign)
  ,  "PaperProps" :: Foreign
  ,  "TransitionComponent" :: (Component Foreign)
  ,  "TransitionProps" :: Foreign
  ,  action :: EventHandler
  ,  anchorEl :: PopoverPropsAnchorEl
  ,  anchorOrigin :: PopoverOrigin
  ,  anchorPosition :: PopoverPosition
  ,  anchorReference :: String
  ,  elevation :: Number
  ,  getContentAnchorEl :: (EventHandler)
  ,  marginThreshold :: Number
  ,  modal :: Boolean
  ,  role :: String
  ,  transformOrigin :: PopoverOrigin
  ,  transitionDuration :: PopoverPropsTransitionDuration
  ,  key :: String
  ,  children :: Array JSX
  )

popover
  :: forall attrs attrs_  
  . Union attrs attrs_ (PopoverProps  )
  => Record attrs
  -> JSX
popover props = element _popover props
 

popover_ :: Array JSX -> JSX
popover_ children = popover { children }  
