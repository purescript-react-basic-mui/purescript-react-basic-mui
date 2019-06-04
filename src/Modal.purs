-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Modal/Modal.d.ts
module MaterialUI.Basic.Modal where 
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic.Events (EventHandler)
import React.Basic (JSX, ReactComponent, element)



foreign import data ModalManager :: Type

foreign import _modal :: forall a. ReactComponent a



type ModalProps_optional  = 
  ( "BackdropComponent" :: JSX
  ,  "BackdropProps" :: Foreign
  ,  closeAfterTransition :: Boolean
  ,  container :: (Object Foreign)
  ,  disableAutoFocus :: Boolean
  ,  disableBackdropClick :: Boolean
  ,  disableEnforceFocus :: Boolean
  ,  disableEscapeKeyDown :: Boolean
  ,  disablePortal :: (Object Foreign)
  ,  disableRestoreFocus :: Boolean
  ,  hideBackdrop :: Boolean
  ,  keepMounted :: Boolean
  ,  manager :: ModalManager
  ,  onBackdropClick :: EventHandler
  ,  onClose :: (Object Foreign)
  ,  onEscapeKeyDown :: EventHandler
  ,  onRendered :: (Object Foreign)
  ,  key :: String
  ,  children :: Array JSX
  )



type ModalProps_required   optional = 
  ( open :: Boolean
  | optional
  )

modal
  :: forall attrs attrs_  
  . Union attrs attrs_ (ModalProps_optional  )
  => Record ((ModalProps_required  ) attrs)
  -> JSX
modal props = element _modal props  
