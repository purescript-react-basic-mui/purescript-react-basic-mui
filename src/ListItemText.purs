-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/ListItemText/ListItemText.d.ts
module MaterialUI.Basic.ListItemText where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _listItemText :: forall a. ReactComponent a



type ListItemTextProps  = 
  ( disableTypography :: Boolean
  ,  inset :: Boolean
  ,  primary :: JSX
  ,  primaryTypographyProps :: Foreign
  ,  secondary :: JSX
  ,  secondaryTypographyProps :: Foreign
  ,  key :: String
  ,  children :: Array JSX
  )

listItemText
  :: forall attrs attrs_  
  . Union attrs attrs_ (ListItemTextProps  )
  => Record attrs
  -> JSX
listItemText props = element _listItemText props
 

listItemText_ :: Array JSX -> JSX
listItemText_ children = listItemText { children }  
