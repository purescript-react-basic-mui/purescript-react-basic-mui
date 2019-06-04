-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/ClickAwayListener/ClickAwayListener.d.ts
module MaterialUI.Basic.ClickAwayListener where 
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)



foreign import data ClickAwayListenerPropsMouseEvent :: Type
foreign import data ClickAwayListenerPropsTouchEvent :: Type

foreign import _clickAwayListener :: forall a. ReactComponent a



type ClickAwayListenerProps_optional  = 
  ( mouseEvent :: ClickAwayListenerPropsMouseEvent
  ,  touchEvent :: ClickAwayListenerPropsTouchEvent
  ,  key :: String
  ,  children :: Array JSX
  )



type ClickAwayListenerProps_required   optional = 
  ( onClickAway :: EventHandler
  | optional
  )

clickAwayListener
  :: forall attrs attrs_  
  . Union attrs attrs_ (ClickAwayListenerProps_optional  )
  => Record ((ClickAwayListenerProps_required  ) attrs)
  -> JSX
clickAwayListener props = element _clickAwayListener props  
