-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/InputAdornment/InputAdornment.d.ts
module MaterialUI.Basic.InputAdornment where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _inputAdornment :: forall a. ReactComponent a



type InputAdornmentProps_optional  = 
  ( component :: JSX
  ,  disablePointerEvents :: Boolean
  ,  disableTypography :: Boolean
  ,  variant :: String
  ,  key :: String
  ,  children :: Array JSX
  )



type InputAdornmentProps_required   optional = 
  ( position :: String
  | optional
  )

inputAdornment
  :: forall attrs attrs_  
  . Union attrs attrs_ (InputAdornmentProps_optional  )
  => Record ((InputAdornmentProps_required  ) attrs)
  -> JSX
inputAdornment props = element _inputAdornment props  
