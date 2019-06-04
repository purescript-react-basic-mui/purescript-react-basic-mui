-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Switch/Switch.d.ts
module MaterialUI.Basic.Switch where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _switch :: forall a. ReactComponent a



type SwitchProps  = 
  ( checkedIcon :: JSX
  ,  color :: String
  ,  icon :: JSX
  ,  key :: String
  ,  children :: Array JSX
  )

switch
  :: forall attrs attrs_  
  . Union attrs attrs_ (SwitchProps  )
  => Record attrs
  -> JSX
switch props = element _switch props
 

switch_ :: Array JSX -> JSX
switch_ children = switch { children }  
