module MUI.Core.CardContent where

import Prelude

import Data.Maybe (Maybe(..))
import Foreign.Object (Object)
import MUI.Core (JSS)
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type CardContentProps =
  ( children :: Maybe (Array JSX)
  , classes :: Maybe (Object JSS)
  , component :: Maybe String
  )

cardContentProps :: { | CardContentProps }
cardContentProps =
  { children : Nothing
  , classes : Nothing 
  , component : Just "div"
  }
  
cardContent :: { | CardContentProps } -> JSX
cardContent props = element _CardContent (unsafeCoerce $ write $ toInternalChildren props)


foreign import _CardContent :: ∀ a. ReactComponent a