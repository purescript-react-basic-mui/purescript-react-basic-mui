-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Dialog/Dialog.d.ts
module MaterialUI.Basic.Dialog where 
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic (Component, JSX, ReactComponent, element)

import MaterialUI.Basic.Paper (PaperProps)


foreign import data DialogPropsMaxWidth :: Type

foreign import _dialog :: forall a. ReactComponent a



type DialogProps  = 
  ( "PaperComponent" :: (Component (Record PaperProps))
  ,  "PaperProps" :: Foreign
  ,  "TransitionComponent" :: (Component Foreign)
  ,  "TransitionProps" :: Foreign
  ,  fullScreen :: Boolean
  ,  fullWidth :: Boolean
  ,  maxWidth :: DialogPropsMaxWidth
  ,  scroll :: String
  ,  transitionDuration :: (Object Foreign)
  ,  key :: String
  ,  children :: Array JSX
  )

dialog
  :: forall attrs attrs_  
  . Union attrs attrs_ (DialogProps  )
  => Record attrs
  -> JSX
dialog props = element _dialog props
 

dialog_ :: Array JSX -> JSX
dialog_ children = dialog { children }  
