-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/CssBaseline/CssBaseline.d.ts
module MaterialUI.Basic.CssBaseline where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _cssBaseline :: forall a. ReactComponent a



type CssBaselineProps  = 
  ( key :: String
  ,  children :: Array JSX
  )

cssBaseline
  :: forall attrs attrs_  
  . Union attrs attrs_ (CssBaselineProps  )
  => Record attrs
  -> JSX
cssBaseline props = element _cssBaseline props
 

cssBaseline_ :: Array JSX -> JSX
cssBaseline_ children = cssBaseline { children }  
