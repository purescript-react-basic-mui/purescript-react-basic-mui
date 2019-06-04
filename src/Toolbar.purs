-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Toolbar/Toolbar.d.ts
module MaterialUI.Basic.Toolbar where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _toolbar :: forall a. ReactComponent a



type ToolbarProps  = 
  ( component :: JSX
  ,  disableGutters :: Boolean
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

toolbar
  :: forall attrs attrs_  
  . Union attrs attrs_ (ToolbarProps  )
  => Record attrs
  -> JSX
toolbar props = element _toolbar props
 

toolbar_ :: Array JSX -> JSX
toolbar_ children = toolbar { children }  
