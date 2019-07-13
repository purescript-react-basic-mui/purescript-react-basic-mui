module MUI.Core.CardContent where

import Prelude

import Data.Maybe (Maybe(..))
import MUI.Core (JSS)
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type CardContentProps =
  ( children :: Maybe (Array JSX)
  , classes :: CardContentClassKey 
  , className :: Maybe String
  , component :: Maybe String
  )

type CardContentClassKey = { root :: Maybe JSS }

classes :: CardContentClassKey 
classes = { root : Nothing }

cardContentProps :: { | CardContentProps }
cardContentProps =
  { children : Nothing
  , classes
  , className : Nothing
  , component : Just "div"
  }
  
cardContent :: { | CardContentProps } -> JSX
cardContent props = element _CardContent (unsafeCoerce $ write $ toInternalChildren props)


foreign import _CardContent :: ∀ a. ReactComponent a