module MUI.Core.ListItemIcon where

import Data.Maybe (Maybe(..))
import MUI.Core (JSS)
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type ListItemIconProps =
  ( children :: Maybe (Array JSX)
  , classes :: ListItemIconClassKey
  )

type ListItemIconClassKey =
  { root :: Maybe JSS
  , alignItemsFlexStart :: Maybe JSS
  }

classes :: ListItemIconClassKey
classes =
  { root : Nothing
  , alignItemsFlexStart : Nothing
  }

listItemIconProps :: { | ListItemIconProps }
listItemIconProps = 
  { children : Nothing
  , classes
  }

listItemIcon :: { | ListItemIconProps } -> JSX
listItemIcon props = do
  let foreignProps = write (toInternalChildren props) 
  element _ListItemIcon (unsafeCoerce foreignProps)



foreign import _ListItemIcon :: ∀ a. ReactComponent a