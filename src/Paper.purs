-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Paper/Paper.d.ts
module MaterialUI.Basic.Paper where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _paper :: forall a. ReactComponent a



type PaperProps  = 
  ( component :: JSX
  ,  elevation :: Number
  ,  square :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

paper
  :: forall attrs attrs_  
  . Union attrs attrs_ (PaperProps  )
  => Record attrs
  -> JSX
paper props = element _paper props
 

paper_ :: Array JSX -> JSX
paper_ children = paper { children }  
