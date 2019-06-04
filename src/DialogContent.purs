-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/DialogContent/DialogContent.d.ts
module MaterialUI.Basic.DialogContent where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _dialogContent :: forall a. ReactComponent a



type DialogContentProps  = 
  ( dividers :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

dialogContent
  :: forall attrs attrs_  
  . Union attrs attrs_ (DialogContentProps  )
  => Record attrs
  -> JSX
dialogContent props = element _dialogContent props
 

dialogContent_ :: Array JSX -> JSX
dialogContent_ children = dialogContent { children }  
