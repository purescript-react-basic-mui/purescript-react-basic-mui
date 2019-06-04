-- /Users/dtwhitney/development/purescript/purescript-react-basic-mui/codegen/node_modules/@material-ui/core/Zoom/Zoom.d.ts
module MaterialUI.Basic.Zoom where 
import Foreign (Foreign)
import Prim.Row (class Union)
import React.Basic.DOM.Internal (CSS)
import React.Basic (JSX, ReactComponent, Ref, element)

import MaterialUI.Basic.Styles.CreateMuiTheme (Theme)




foreign import _zoom :: forall a. ReactComponent a


-- | - `appear`
-- |        Normally a component is not transitioned if it is shown when the
-- |        `<Transition>` component mounts. If you want to transition on the first
-- |        mount set  appear to true, and the component will transition in as soon
-- |        as the `<Transition>` mounts. Note: there are no specific "appear" states.
-- |        appear only adds an additional enter transition.
-- | - `enter`
-- |        Enable or disable enter transitions.
-- | - `exit`
-- |        Enable or disable exit transitions.

type ZoomProps  = 
  ( appear :: Boolean
  ,  enter :: Boolean
  ,  exit :: Boolean
  ,  ref :: (Ref Foreign)
  ,  style :: CSS
  ,  theme :: Theme
  ,  key :: String
  ,  children :: Array JSX
  )

zoom
  :: forall attrs attrs_  
  . Union attrs attrs_ (ZoomProps  )
  => Record attrs
  -> JSX
zoom props = element _zoom props
 

zoom_ :: Array JSX -> JSX
zoom_ children = zoom { children }  
