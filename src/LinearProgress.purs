-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/LinearProgress/LinearProgress.d.ts
module MaterialUI.Basic.LinearProgress where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _linearProgress :: forall a. ReactComponent a



type LinearProgressProps  = 
  ( color :: String
  ,  value :: Number
  ,  valueBuffer :: Number
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

linearProgress
  :: forall attrs attrs_  
  . Union attrs attrs_ (LinearProgressProps  )
  => Record attrs
  -> JSX
linearProgress props = element _linearProgress props
 

linearProgress_ :: Array JSX -> JSX
linearProgress_ children = linearProgress { children }  
