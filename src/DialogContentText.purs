-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/DialogContentText/DialogContentText.d.ts
module MaterialUI.Basic.DialogContentText where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _dialogContentText :: forall a. ReactComponent a



type DialogContentTextProps  = 
  ( key :: String
  ,  children :: Array JSX
  )

dialogContentText
  :: forall attrs attrs_  
  . Union attrs attrs_ (DialogContentTextProps  )
  => Record attrs
  -> JSX
dialogContentText props = element _dialogContentText props
 

dialogContentText_ :: Array JSX -> JSX
dialogContentText_ children = dialogContentText { children }  
