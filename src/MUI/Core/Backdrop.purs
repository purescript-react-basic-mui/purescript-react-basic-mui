module MUI.Core.Backdrop where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import MUI.Core (JSS)
import MUI.Core.Internal (addEndListener, onClick, onEnter, onEntered, onEntering, onExit, onExited, onExiting, theme, toInternalChildren)
import MUI.Core.Styles.CreateMuiTheme (Theme)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type BackdropProps =
  ( invisible :: Maybe Boolean
  , children :: Maybe (Array JSX)
  , classes :: BackdropClassKey
  , className :: Maybe String
  , onClick :: Maybe EventHandler
  , open :: Boolean
  , transitionDuration :: Maybe { enter :: Maybe Number, exit :: Maybe Number }
  , ref :: Maybe Foreign
  , theme :: Maybe Theme
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

backdropProps :: { | BackdropProps }
backdropProps =
  { invisible : Just false
  , children : Nothing
  , classes
  , className : Nothing
  , onClick : Nothing
  , open : false
  , transitionDuration : Nothing
  , ref : Nothing
  , theme : Nothing
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


type BackdropClassKey = 
  { root :: Maybe JSS
  , invisible :: Maybe JSS
  }

classes :: BackdropClassKey 
classes =
  { root : Nothing
  , invisible : Nothing
  }

backdrop :: { | BackdropProps } -> JSX
backdrop props = do
  let foreignProps = write 
        $ (onClick <<< onEnter <<< onEntering <<< onEntered <<< onExit <<< onExiting <<< onExited <<< addEndListener <<< theme <<< toInternalChildren) 
        $ props
  element _Backdrop (unsafeCoerce foreignProps)

foreign import _Backdrop :: ∀ a. ReactComponent a

