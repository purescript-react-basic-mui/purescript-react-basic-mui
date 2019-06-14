module React.Basic.MUI.Zoom where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Styles.CreateMuiTheme (Theme)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)
import React.Basic (element, ReactComponent, JSX)

type ZoomProps_optional =
  ( ref :: Foreign
  , theme :: Theme 
  , style :: CSS
  , appear :: Boolean
  , enter :: Boolean
  , exit :: Boolean
  , onEnter :: EventHandler
  , onEntering :: EventHandler
  , onEntered :: EventHandler
  , onExit :: EventHandler
  , onExiting :: EventHandler
  , onExited :: EventHandler
  , in :: Boolean
  , mountOnEnter :: Boolean
  , unmountOnExit :: Boolean
  , timeout :: Foreign
  , addEndListener :: EventHandler
  )

foreign import data ZoomProps :: Type 

zoomProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ZoomProps_optional)
  => Record (attrs)
  -> ZoomProps
zoomProps = unsafeCoerce

zoom
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ZoomProps_optional)
  => Record (attrs)
  -> JSX
zoom = element _Zoom
foreign import _Zoom :: forall a. ReactComponent a 