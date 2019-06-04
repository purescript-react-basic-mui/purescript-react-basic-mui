-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/DialogTitle/DialogTitle.d.ts
module MaterialUI.Basic.DialogTitle where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _dialogTitle :: forall a. ReactComponent a



type DialogTitleProps  = 
  ( disableTypography :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

dialogTitle
  :: forall attrs attrs_  
  . Union attrs attrs_ (DialogTitleProps  )
  => Record attrs
  -> JSX
dialogTitle props = element _dialogTitle props
 

dialogTitle_ :: Array JSX -> JSX
dialogTitle_ children = dialogTitle { children }  
