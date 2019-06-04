-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/ListSubheader/ListSubheader.d.ts
module MaterialUI.Basic.ListSubheader where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _listSubheader :: forall a. ReactComponent a



type ListSubheaderProps  = 
  ( color :: String
  ,  component :: JSX
  ,  disableGutters :: Boolean
  ,  disableSticky :: Boolean
  ,  inset :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

listSubheader
  :: forall attrs attrs_  
  . Union attrs attrs_ (ListSubheaderProps  )
  => Record attrs
  -> JSX
listSubheader props = element _listSubheader props
 

listSubheader_ :: Array JSX -> JSX
listSubheader_ children = listSubheader { children }  
