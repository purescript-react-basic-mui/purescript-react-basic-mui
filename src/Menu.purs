-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Menu/Menu.d.ts
module MaterialUI.Basic.Menu where 
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data MenuPropsTransitionDuration :: Type

foreign import _menu :: forall a. ReactComponent a



type MenuProps  = 
  ( "MenuListProps" :: Foreign
  ,  "PaperProps" :: Foreign
  ,  "PopoverClasses" :: (Object Foreign)
  ,  disableAutoFocusItem :: Boolean
  ,  transitionDuration :: MenuPropsTransitionDuration
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

menu
  :: forall attrs attrs_  
  . Union attrs attrs_ (MenuProps  )
  => Record attrs
  -> JSX
menu props = element _menu props
 

menu_ :: Array JSX -> JSX
menu_ children = menu { children }  
