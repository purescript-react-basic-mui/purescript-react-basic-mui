module MUI.Core.Drawer where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Foreign (Foreign)
import Foreign.Object (Object)
import Foreign.Object as Object
import MUI.Core (JSS)
import MUI.Core.Internal (onClose, toInternalChildren)
import MUI.Core.Slide (SlideProps, slideProps)
import MUI.Core.Slide as Slide
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Record (modify)
import Record as Record
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type ModalProps = Object Foreign
type PaperProps = Object Foreign

type DrawerProps =
  ( anchor :: Maybe String
  , children :: Maybe (Array JSX)
  , classes :: DrawerClassKey
  , className :: Maybe String
  , elevation :: Maybe Number
  , "ModalProps" :: ModalProps
  , onClose :: Maybe EventHandler
  , open :: Maybe Boolean
  , "PaperProps" :: PaperProps
  , "SlideProps" :: { | SlideProps }
  , transitionDuration :: Maybe { enter :: Maybe Number, exit :: Maybe Number }
  , variant :: Maybe String
  )

drawerProps :: { | DrawerProps }
drawerProps =
  { anchor : Just "left"
  , children : Nothing
  , classes
  , className : Nothing
  , elevation : Just 16.0
  , "ModalProps" : Object.empty
  , onClose : Nothing
  , open : Just false
  , "PaperProps" : Object.empty
  , "SlideProps" : slideProps
  , transitionDuration : Nothing
  , variant : Just "temporary"
  }

type DrawerClassKey =
  { root :: Maybe JSS
  , docked :: Maybe JSS
  , paper :: Maybe JSS
  , paperAnchorLeft :: Maybe JSS
  , paperAnchorRight :: Maybe JSS
  , paperAnchorTop :: Maybe JSS
  , paperAnchorBottom :: Maybe JSS
  , paperAnchorDockedLeft :: Maybe JSS
  , paperAnchorDockedTop :: Maybe JSS
  , paperAnchorDockedRight :: Maybe JSS
  , paperAnchorDockedBottom :: Maybe JSS
  , modal :: Maybe JSS
  }

classes :: DrawerClassKey 
classes =
  { root : Nothing
  , docked : Nothing
  , paper : Nothing
  , paperAnchorLeft : Nothing
  , paperAnchorRight : Nothing
  , paperAnchorTop : Nothing
  , paperAnchorBottom : Nothing
  , paperAnchorDockedLeft : Nothing
  , paperAnchorDockedTop : Nothing
  , paperAnchorDockedRight : Nothing
  , paperAnchorDockedBottom : Nothing
  , modal : Nothing
  }

drawer :: { | DrawerProps } -> JSX
drawer props = do 
  let foreignSlideProps = Slide.propsToForeign props."SlideProps"
      newProps = Record.set (SProxy :: SProxy "SlideProps") foreignSlideProps props
  element _Drawer (unsafeCoerce $ write $ (onClose <<< toInternalChildren) newProps)

foreign import _Drawer :: ∀ a. ReactComponent a