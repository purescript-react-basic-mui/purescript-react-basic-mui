module React.Basic.MUI.Fade where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)
import React.Basic (element, ReactComponent, JSX)

type FadeProps_optional =
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

foreign import data FadeProps :: Type 

fadeProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (FadeProps_optional)
  => Record (attrs)
  -> FadeProps
fadeProps = unsafeCoerce

fade
  :: ∀ attrs attrs_
   . Union attrs attrs_ (FadeProps_optional)
  => Record (attrs)
  -> JSX
fade = element _Fade
foreign import _Fade :: forall a. ReactComponent a 