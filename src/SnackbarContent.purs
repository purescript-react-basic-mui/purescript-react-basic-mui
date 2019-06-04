-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/SnackbarContent/SnackbarContent.d.ts
module MaterialUI.Basic.SnackbarContent where 
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)





foreign import _snackbarContent :: forall a. ReactComponent a



type SnackbarContentProps  = 
  ( action :: JSX
  ,  message :: JSX
  ,  key :: String
  ,  children :: Array JSX
  )

snackbarContent
  :: forall attrs attrs_  
  . Union attrs attrs_ (SnackbarContentProps  )
  => Record attrs
  -> JSX
snackbarContent props = element _snackbarContent props
 

snackbarContent_ :: Array JSX -> JSX
snackbarContent_ children = snackbarContent { children }  
