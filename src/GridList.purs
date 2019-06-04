-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/GridList/GridList.d.ts
module MaterialUI.Basic.GridList where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _gridList :: forall a. ReactComponent a



type GridListProps  = 
  ( cellHeight :: String
  ,  cols :: Number
  ,  component :: JSX
  ,  spacing :: Number
  ,  key :: String
  ,  children :: Array JSX
  )

gridList
  :: forall attrs attrs_  
  . Union attrs attrs_ (GridListProps  )
  => Record attrs
  -> JSX
gridList props = element _gridList props
 

gridList_ :: Array JSX -> JSX
gridList_ children = gridList { children }  
