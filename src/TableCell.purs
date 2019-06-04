-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/TableCell/TableCell.d.ts
module MaterialUI.Basic.TableCell where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data TableCellPropsSortDirection :: Type

foreign import _tableCell :: forall a. ReactComponent a



type TableCellProps  = 
  ( align :: String
  ,  component :: JSX
  ,  padding :: String
  ,  size :: String
  ,  sortDirection :: TableCellPropsSortDirection
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

tableCell
  :: forall attrs attrs_  
  . Union attrs attrs_ (TableCellProps  )
  => Record attrs
  -> JSX
tableCell props = element _tableCell props
 

tableCell_ :: Array JSX -> JSX
tableCell_ children = tableCell { children }  
