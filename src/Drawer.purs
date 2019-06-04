-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Drawer/Drawer.d.ts
module MaterialUI.Basic.Drawer where 
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)

import MaterialUI.Basic.Styles.CreateMuiTheme (Theme)




foreign import _drawer :: forall a. ReactComponent a



type DrawerProps  = 
  ( "ModalProps" :: Foreign
  ,  "PaperProps" :: Foreign
  ,  "SlideProps" :: Foreign
  ,  anchor :: String
  ,  elevation :: Number
  ,  open :: Boolean
  ,  theme :: Theme
  ,  transitionDuration :: (Object Foreign)
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

drawer
  :: forall attrs attrs_  
  . Union attrs attrs_ (DrawerProps  )
  => Record attrs
  -> JSX
drawer props = element _drawer props
 

drawer_ :: Array JSX -> JSX
drawer_ children = drawer { children }  
