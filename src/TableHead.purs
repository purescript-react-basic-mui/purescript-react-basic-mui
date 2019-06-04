-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/TableHead/TableHead.d.ts
module MaterialUI.Basic.TableHead where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _tableHead :: forall a. ReactComponent a



type TableHeadProps  = 
  ( component :: JSX
  ,  key :: String
  ,  children :: Array JSX
  )

tableHead
  :: forall attrs attrs_  
  . Union attrs attrs_ (TableHeadProps  )
  => Record attrs
  -> JSX
tableHead props = element _tableHead props
 

tableHead_ :: Array JSX -> JSX
tableHead_ children = tableHead { children }  
