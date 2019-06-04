-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Snackbar/Snackbar.d.ts
module MaterialUI.Basic.Snackbar where 
import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Foreign.Object (Object)
import Prelude
import Prim.Row (class Union)
import React.Basic (Component, JSX, ReactComponent, element)





foreign import _snackbar :: forall a. ReactComponent a

type SnackbarOrigin  = {
    horizontal :: String
  , vertical :: String
}

  


type SnackbarProps_optional  = 
  ( "ClickAwayListenerProps" :: Foreign
  ,  "ContentProps" :: Foreign
  ,  "TransitionComponent" :: (Component Foreign)
  ,  "TransitionProps" :: Foreign
  ,  action :: (Object Foreign)
  ,  anchorOrigin :: SnackbarOrigin
  ,  autoHideDuration :: Number
  ,  disableWindowBlurListener :: Boolean
  ,  message :: (Object Foreign)
  ,  onClose :: (EffectFn2 Foreign String Unit)
  ,  onMouseEnter :: Foreign
  ,  onMouseLeave :: Foreign
  ,  resumeHideDuration :: Number
  ,  transitionDuration :: (Object Foreign)
  ,  key :: String
  ,  children :: Array JSX
  )



type SnackbarProps_required   optional = 
  ( open :: Boolean
  | optional
  )

snackbar
  :: forall attrs attrs_  
  . Union attrs attrs_ (SnackbarProps_optional  )
  => Record ((SnackbarProps_required  ) attrs)
  -> JSX
snackbar props = element _snackbar props  
