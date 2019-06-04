-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/DialogActions/DialogActions.d.ts
module MaterialUI.Basic.DialogActions where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _dialogActions :: forall a. ReactComponent a



type DialogActionsProps  = 
  ( disableSpacing :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )

dialogActions
  :: forall attrs attrs_  
  . Union attrs attrs_ (DialogActionsProps  )
  => Record attrs
  -> JSX
dialogActions props = element _dialogActions props
 

dialogActions_ :: Array JSX -> JSX
dialogActions_ children = dialogActions { children }  
