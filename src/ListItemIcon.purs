-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/ListItemIcon/ListItemIcon.d.ts
module MaterialUI.Basic.ListItemIcon where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _listItemIcon :: forall a. ReactComponent a



type ListItemIconProps  = 
  ( key :: String
  ,  children :: Array JSX
  )

listItemIcon
  :: forall attrs attrs_  
  . Union attrs attrs_ (ListItemIconProps  )
  => Record attrs
  -> JSX
listItemIcon props = element _listItemIcon props
 

listItemIcon_ :: Array JSX -> JSX
listItemIcon_ children = listItemIcon { children }  
