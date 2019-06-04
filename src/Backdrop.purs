-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Backdrop/Backdrop.d.ts
module MaterialUI.Basic.Backdrop where 
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)





foreign import _backdrop :: forall a. ReactComponent a



type BackdropProps_optional  = 
  ( invisible :: Boolean
  ,  onClick :: EventHandler
  ,  transitionDuration :: (Object Foreign)
  ,  key :: String
  ,  children :: Array JSX
  )



type BackdropProps_required   optional = 
  ( open :: Boolean
  | optional
  )

backdrop
  :: forall attrs attrs_  
  . Union attrs attrs_ (BackdropProps_optional  )
  => Record ((BackdropProps_required  ) attrs)
  -> JSX
backdrop props = element _backdrop props  
