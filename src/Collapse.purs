module React.Basic.MUI.Collapse where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Transitions.Transition (TransitionProps)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)

type CollapseProps_optional =
  ( children :: JSX
  , collapsedHeight :: String
  , component :: JSX
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
  , classes :: Foreign
  , innerRef :: Foreign
  , className :: String
  , ref :: Foreign
  )

foreign import data CollapseProps :: Type 

collapseProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (CollapseProps_optional)
  => Record (attrs)
  -> CollapseProps
collapseProps = unsafeCoerce

type CollapseClassKey = Foreign

collapse
  :: ∀ attrs attrs_
   . Union attrs attrs_ (CollapseProps_optional)
  => Record (attrs)
  -> JSX
collapse = element _Collapse
foreign import _Collapse :: forall a. ReactComponent a 