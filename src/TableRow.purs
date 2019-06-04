-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/TableRow/TableRow.d.ts
module MaterialUI.Basic.TableRow where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _tableRow :: forall a. ReactComponent a



type TableRowProps  = 
  ( component :: JSX
  ,  hover :: Boolean
  ,  selected :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

tableRow
  :: forall attrs attrs_  
  . Union attrs attrs_ (TableRowProps  )
  => Record attrs
  -> JSX
tableRow props = element _tableRow props
 

tableRow_ :: Array JSX -> JSX
tableRow_ children = tableRow { children }  
