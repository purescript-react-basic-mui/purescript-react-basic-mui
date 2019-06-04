-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/FormGroup/FormGroup.d.ts
module MaterialUI.Basic.FormGroup where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _formGroup :: forall a. ReactComponent a



type FormGroupProps  = 
  ( row :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

formGroup
  :: forall attrs attrs_  
  . Union attrs attrs_ (FormGroupProps  )
  => Record attrs
  -> JSX
formGroup props = element _formGroup props
 

formGroup_ :: Array JSX -> JSX
formGroup_ children = formGroup { children }  
