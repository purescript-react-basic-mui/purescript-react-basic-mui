-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/OutlinedInput/OutlinedInput.d.ts
module MaterialUI.Basic.OutlinedInput where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _outlinedInput :: forall a. ReactComponent a



type OutlinedInputProps_optional  = 
  ( notched :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )



type OutlinedInputProps_required   optional = 
  ( labelWidth :: Number
  | optional
  )

outlinedInput
  :: forall attrs attrs_  
  . Union attrs attrs_ (OutlinedInputProps_optional  )
  => Record ((OutlinedInputProps_required  ) attrs)
  -> JSX
outlinedInput props = element _outlinedInput props  
