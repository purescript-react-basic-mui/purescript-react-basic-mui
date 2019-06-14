module React.Basic.MUI.Grow where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Styles.CreateMuiTheme (Theme)
import React.Basic.MUI.Transitions.Transition (TransitionProps)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)
import React.Basic (element, ReactComponent, JSX)

type GrowProps_optional =
  ( ref :: Foreign
  , theme :: Theme 
  , timeout :: Foreign
  , style :: CSS
  , onEnter :: EventHandler
  , onEntering :: EventHandler
  , onEntered :: EventHandler
  , onExit :: EventHandler
  , onExiting :: EventHandler
  , onExited :: EventHandler
  , in :: Boolean
  , mountOnEnter :: Boolean
  , unmountOnExit :: Boolean
  , addEndListener :: EventHandler
  , appear :: Boolean
  , enter :: Boolean
  , exit :: Boolean
  )

foreign import data GrowProps :: Type 

growProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (GrowProps_optional)
  => Record (attrs)
  -> GrowProps
growProps = unsafeCoerce

grow
  :: ∀ attrs attrs_
   . Union attrs attrs_ (GrowProps_optional)
  => Record (attrs)
  -> JSX
grow = element _Grow
foreign import _Grow :: forall a. ReactComponent a 