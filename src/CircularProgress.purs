-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/CircularProgress/CircularProgress.d.ts
module MaterialUI.Basic.CircularProgress where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _circularProgress :: forall a. ReactComponent a



type CircularProgressProps  = 
  ( color :: String
  ,  disableShrink :: Boolean
  ,  size :: String
  ,  thickness :: Number
  ,  value :: Number
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

circularProgress
  :: forall attrs attrs_  
  . Union attrs attrs_ (CircularProgressProps  )
  => Record attrs
  -> JSX
circularProgress props = element _circularProgress props
 

circularProgress_ :: Array JSX -> JSX
circularProgress_ children = circularProgress { children }  
