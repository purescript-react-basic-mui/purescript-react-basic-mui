-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Input/Input.d.ts
module MaterialUI.Basic.Input where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _input :: forall a. ReactComponent a



type InputProps  = 
  ( disableUnderline :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

input
  :: forall attrs attrs_  
  . Union attrs attrs_ (InputProps  )
  => Record attrs
  -> JSX
input props = element _input props
 

input_ :: Array JSX -> JSX
input_ children = input { children }  
