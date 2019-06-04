-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/ListItemAvatar/ListItemAvatar.d.ts
module MaterialUI.Basic.ListItemAvatar where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _listItemAvatar :: forall a. ReactComponent a



type ListItemAvatarProps  = 
  ( key :: String
  ,  children :: Array JSX
  )

listItemAvatar
  :: forall attrs attrs_  
  . Union attrs attrs_ (ListItemAvatarProps  )
  => Record attrs
  -> JSX
listItemAvatar props = element _listItemAvatar props
 

listItemAvatar_ :: Array JSX -> JSX
listItemAvatar_ children = listItemAvatar { children }  
