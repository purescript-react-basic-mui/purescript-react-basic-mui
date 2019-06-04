-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/TableSortLabel/TableSortLabel.d.ts
module MaterialUI.Basic.TableSortLabel where 
import Prim.Row (class Union)
import React.Basic (Component, JSX, ReactComponent, element)

import MaterialUI.Basic.SvgIcon (SvgIconProps)




foreign import _tableSortLabel :: forall a. ReactComponent a



type TableSortLabelProps  = 
  ( "IconComponent" :: (Component (Record SvgIconProps))
  ,  active :: Boolean
  ,  direction :: String
  ,  hideSortIcon :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

tableSortLabel
  :: forall attrs attrs_  
  . Union attrs attrs_ (TableSortLabelProps  )
  => Record attrs
  -> JSX
tableSortLabel props = element _tableSortLabel props
 

tableSortLabel_ :: Array JSX -> JSX
tableSortLabel_ children = tableSortLabel { children }  
