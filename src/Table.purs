-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Table/Table.d.ts
module MaterialUI.Basic.Table where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _table :: forall a. ReactComponent a



type TableProps  = 
  ( component :: JSX
  ,  padding :: String
  ,  size :: String
  ,  key :: String
  ,  children :: Array JSX
  )

table
  :: forall attrs attrs_  
  . Union attrs attrs_ (TableProps  )
  => Record attrs
  -> JSX
table props = element _table props
 

table_ :: Array JSX -> JSX
table_ children = table { children }  
