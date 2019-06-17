module React.Basic.MUI.Core.Transitions.Transition where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.Events (EventHandler)
import React.Basic.DOM.Internal (CSS)

type TransitionHandlerKeys = Foreign

type TransitionHandlerProps = EventHandler

type TransitionKeys = Foreign

type TransitionProps_optional =
  ( style :: CSS
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

foreign import data TransitionProps :: Type 

transitionProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TransitionProps_optional )
  => Record (attrs)
  -> TransitionProps
transitionProps = unsafeCoerce