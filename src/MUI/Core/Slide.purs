module MUI.Core.Slide where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import MUI.Core.Internal (addEndListener, onEnter, onEntered, onEntering, onExit, onExited, onExiting, toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type SlideProps =
  ( children :: Maybe (Array JSX)
  , direction :: Maybe String
  , in :: Maybe Boolean
  , timeout :: Maybe Number
  , onEnter :: Maybe EventHandler
  , onEntering :: Maybe EventHandler
  , onEntered :: Maybe EventHandler
  , onExit :: Maybe EventHandler
  , onExiting :: Maybe EventHandler
  , onExited :: Maybe EventHandler
  , mountOnEnter :: Maybe Boolean
  , unmountOnExit :: Maybe Boolean
  , addEndListener :: Maybe EventHandler
  )

slideProps :: { | SlideProps }
slideProps =
  { children : Nothing
  , direction : Just "down"
  , in : Just false
  , timeout : Nothing
  , onEnter : Nothing
  , onEntering : Nothing
  , onEntered : Nothing
  , onExit : Nothing
  , onExiting : Nothing
  , onExited : Nothing
  , mountOnEnter : Just true
  , unmountOnExit : Just false
  , addEndListener : Nothing
  }

propsToForeign :: { | SlideProps } -> Foreign
propsToForeign props = 
  write $ (onEnter <<< onEntering <<< onEntered <<< onExit <<< onExiting <<< onExited <<< addEndListener <<< toInternalChildren) props

slide :: { | SlideProps } -> JSX
slide props = element _Slide $ unsafeCoerce $ propsToForeign props


foreign import _Slide :: ∀ a. ReactComponent a