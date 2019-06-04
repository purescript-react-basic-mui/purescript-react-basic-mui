-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/GridListTile/GridListTile.d.ts
module MaterialUI.Basic.GridListTile where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _gridListTile :: forall a. ReactComponent a



type GridListTileProps  = 
  ( cols :: Number
  ,  component :: JSX
  ,  rows :: Number
  ,  key :: String
  ,  children :: Array JSX
  )

gridListTile
  :: forall attrs attrs_  
  . Union attrs attrs_ (GridListTileProps  )
  => Record attrs
  -> JSX
gridListTile props = element _gridListTile props
 

gridListTile_ :: Array JSX -> JSX
gridListTile_ children = gridListTile { children }  
