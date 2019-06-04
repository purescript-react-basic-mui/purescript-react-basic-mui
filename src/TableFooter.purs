-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/TableFooter/TableFooter.d.ts
module MaterialUI.Basic.TableFooter where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _tableFooter :: forall a. ReactComponent a



type TableFooterProps  = 
  ( component :: JSX
  ,  key :: String
  ,  children :: Array JSX
  )

tableFooter
  :: forall attrs attrs_  
  . Union attrs attrs_ (TableFooterProps  )
  => Record attrs
  -> JSX
tableFooter props = element _tableFooter props
 

tableFooter_ :: Array JSX -> JSX
tableFooter_ children = tableFooter { children }  
