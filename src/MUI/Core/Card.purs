module MUI.Core.Card where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import MUI.Core (JSS)
import MUI.Core.Internal (toInternalChildren) 
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type CardProps =
  ( children :: Maybe (Array JSX)
  , classes :: Maybe (Object JSS)
  , component :: Maybe String
  , elevation :: Maybe Number
  , square :: Maybe Boolean
  , raised :: Maybe Boolean
  )

cardProps :: { | CardProps }
cardProps = 
  { children : Nothing
  , classes : Nothing
  , component : Just "div"
  , elevation : Just 1.0
  , square : Just false
  , raised : Just false
  }

card :: { | CardProps } -> JSX
card props = element _Card (unsafeCoerce $ write $ toInternalChildren props)

foreign import _Card :: ∀ a. ReactComponent a