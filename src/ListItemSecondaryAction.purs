-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/ListItemSecondaryAction/ListItemSecondaryAction.d.ts
module MaterialUI.Basic.ListItemSecondaryAction where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _listItemSecondaryAction :: forall a. ReactComponent a



type ListItemSecondaryActionProps  = 
  ( key :: String
  ,  children :: Array JSX
  )

listItemSecondaryAction
  :: forall attrs attrs_  
  . Union attrs attrs_ (ListItemSecondaryActionProps  )
  => Record attrs
  -> JSX
listItemSecondaryAction props = element _listItemSecondaryAction props
 

listItemSecondaryAction_ :: Array JSX -> JSX
listItemSecondaryAction_ children = listItemSecondaryAction { children }  
