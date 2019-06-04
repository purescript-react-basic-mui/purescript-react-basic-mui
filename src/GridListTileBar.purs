-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/GridListTileBar/GridListTileBar.d.ts
module MaterialUI.Basic.GridListTileBar where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _gridListTileBar :: forall a. ReactComponent a



type GridListTileBarProps  = 
  ( actionIcon :: JSX
  ,  actionPosition :: String
  ,  subtitle :: JSX
  ,  title :: JSX
  ,  titlePosition :: String
  ,  key :: String
  ,  children :: Array JSX
  )

gridListTileBar
  :: forall attrs attrs_  
  . Union attrs attrs_ (GridListTileBarProps  )
  => Record attrs
  -> JSX
gridListTileBar props = element _gridListTileBar props
 

gridListTileBar_ :: Array JSX -> JSX
gridListTileBar_ children = gridListTileBar { children }  
