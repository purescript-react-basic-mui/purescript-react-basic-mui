-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Grid/Grid.d.ts
module MaterialUI.Basic.Grid where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data GridPropsComponent :: Type

foreign import _grid :: forall a. ReactComponent a



type GridProps  = 
  ( alignContent :: String
  ,  alignItems :: String
  ,  component :: GridPropsComponent
  ,  container :: Boolean
  ,  direction :: String
  ,  item :: Boolean
  ,  justify :: String
  ,  spacing :: Number
  ,  wrap :: String
  ,  zeroMinWidth :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

grid
  :: forall attrs attrs_  
  . Union attrs attrs_ (GridProps  )
  => Record attrs
  -> JSX
grid props = element _grid props
 

grid_ :: Array JSX -> JSX
grid_ children = grid { children }  
