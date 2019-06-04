-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/TableBody/TableBody.d.ts
module MaterialUI.Basic.TableBody where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _tableBody :: forall a. ReactComponent a



type TableBodyProps  = 
  ( component :: JSX
  ,  key :: String
  ,  children :: Array JSX
  )

tableBody
  :: forall attrs attrs_  
  . Union attrs attrs_ (TableBodyProps  )
  => Record attrs
  -> JSX
tableBody props = element _tableBody props
 

tableBody_ :: Array JSX -> JSX
tableBody_ children = tableBody { children }  
