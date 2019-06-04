-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/FormHelperText/FormHelperText.d.ts
module MaterialUI.Basic.FormHelperText where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data Dense :: Type

foreign import _formHelperText :: forall a. ReactComponent a



type FormHelperTextProps  = 
  ( component :: JSX
  ,  disabled :: Boolean
  ,  error :: Boolean
  ,  filled :: Boolean
  ,  focused :: Boolean
  ,  margin :: Dense
  ,  required :: Boolean
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )

formHelperText
  :: forall attrs attrs_  
  . Union attrs attrs_ (FormHelperTextProps  )
  => Record attrs
  -> JSX
formHelperText props = element _formHelperText props
 

formHelperText_ :: Array JSX -> JSX
formHelperText_ children = formHelperText { children }  
