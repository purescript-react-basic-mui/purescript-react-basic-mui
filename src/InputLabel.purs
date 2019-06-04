-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/InputLabel/InputLabel.d.ts
module MaterialUI.Basic.InputLabel where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _inputLabel :: forall a. ReactComponent a



type InputLabelProps  = 
  ( disableAnimation :: Boolean
  ,  disabled :: Boolean
  ,  error :: Boolean
  ,  focused :: Boolean
  ,  required :: Boolean
  ,  shrink :: Boolean
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

inputLabel
  :: forall attrs attrs_  
  . Union attrs attrs_ (InputLabelProps  )
  => Record attrs
  -> JSX
inputLabel props = element _inputLabel props
 

inputLabel_ :: Array JSX -> JSX
inputLabel_ children = inputLabel { children }  
