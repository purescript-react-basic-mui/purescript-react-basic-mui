-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Popper/Popper.d.ts
module MaterialUI.Basic.Popper where 
import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)



foreign import data PopperPropsAnchorEl :: Type
foreign import data PopperPropsChildren :: Type

foreign import _popper :: forall a. ReactComponent a



type PopperProps_optional  = 
  ( anchorEl :: PopperPropsAnchorEl
  ,  container :: (Object Foreign)
  ,  disablePortal :: (Object Foreign)
  ,  keepMounted :: Boolean
  ,  modifiers :: (Object Foreign)
  ,  placement :: String
  ,  popperOptions :: (Object Foreign)
  ,  transition :: Boolean
  ,  key :: String
  ,  children :: Array JSX
  )



type PopperProps_required   optional = 
  ( open :: Boolean
  | optional
  )

popper
  :: forall attrs attrs_  
  . Union attrs attrs_ (PopperProps_optional  )
  => Record ((PopperProps_required  ) attrs)
  -> JSX
popper props = element _popper props  
