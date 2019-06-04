-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Checkbox/Checkbox.d.ts
module MaterialUI.Basic.Checkbox where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _checkbox :: forall a. ReactComponent a



type CheckboxProps  = 
  ( checkedIcon :: JSX
  ,  color :: String
  ,  icon :: JSX
  ,  indeterminate :: Boolean
  ,  indeterminateIcon :: JSX
  ,  key :: String
  ,  children :: Array JSX
  )

checkbox
  :: forall attrs attrs_  
  . Union attrs attrs_ (CheckboxProps  )
  => Record attrs
  -> JSX
checkbox props = element _checkbox props
 

checkbox_ :: Array JSX -> JSX
checkbox_ children = checkbox { children }  
