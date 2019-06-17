module React.Basic.MUI.Core.Slide where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.Styles.CreateMuiTheme (Theme)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)
import React.Basic (element, ReactComponent, JSX)

type SlideProps_required  optional =
  ( direction :: Foreign
  | optional )

type SlideProps_optional =
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

foreign import data SlideProps :: Type 

slideProps
  :: ∀ attrs attrs_
   . Union attrs attrs_ (SlideProps_optional )
  => Record (SlideProps_required attrs)
  -> SlideProps
slideProps = unsafeCoerce

slide
  :: ∀ attrs attrs_
   . Union attrs attrs_ (SlideProps_optional )
  => Record (SlideProps_required attrs)
  -> JSX
slide = element _Slide
foreign import _Slide :: forall a. ReactComponent a 