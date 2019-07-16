module MUI.Core.Slide where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

type SlideProps =
  ( children :: Array JSX
  , direction :: String
  , in :: Boolean
  , timeout :: Number
  , onEnter :: EventHandler
  , onEntering :: EventHandler
  , onEntered :: EventHandler
  , onExit :: EventHandler
  , onExiting :: EventHandler
  , onExited :: EventHandler
  , mountOnEnter :: Boolean
  , unmountOnExit :: Boolean
  , addEndListener :: EventHandler
  )

foreign import data SlidePropsPartial :: Type

slidePropsPartial
  :: ∀ props props_
  . Union props props_ SlideProps
  => Record props 
  -> SlidePropsPartial
slidePropsPartial = unsafeCoerce


slide
  :: ∀ props props_
  . Union props props_ SlideProps
  => Record props 
  -> JSX
slide = element _Slide

foreign import _Slide :: ∀ a. ReactComponent a