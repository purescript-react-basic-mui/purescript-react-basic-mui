module MUI.Core.ListItem where

import Control.Monad.ST.Internal (write)
import Data.Maybe (Maybe(..))
import Foreign (Foreign)
import Foreign.Object (Object)
import MUI.Core (JSS)
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type ContainerProps = Object Foreign

type ListItemProps = 
  ( alignItems :: Maybe String
  , autoFocus :: Maybe Boolean
  , button :: Maybe Boolean
  , children :: Maybe (Array JSX)
  , classes :: ListItemClassKey
  , "ContainerComponent" :: Maybe String
  , "ContainerProps" :: Maybe ContainerProps
  , dense :: Maybe Boolean
  , disabled :: Maybe Boolean
  , disableGutters :: Maybe Boolean
  , divider :: Maybe Boolean
  , selected  :: Maybe Boolean
  )

listItemProps :: { | ListItemProps }
listItemProps = 
  { alignItems : Just "center" 
  , autoFocus : Just false
  , button : Just false
  , children : Nothing
  , classes
  , "ContainerComponent" : Just "li"
  , "ContainerProps" : Nothing
  , dense : Nothing
  , disabled : Just false
  , disableGutters : Nothing
  , divider : Nothing
  , selected : Nothing
  }

type ListItemClassKey =
  { root :: Maybe JSS
  , container :: Maybe JSS
  , focusVisible :: Maybe JSS
  , dense :: Maybe JSS
  , alignItemsFlexStart :: Maybe JSS
  , disabled :: Maybe JSS
  , divider :: Maybe JSS
  , gutters :: Maybe JSS
  , button :: Maybe JSS
  , secondaryAction :: Maybe JSS
  , selected :: Maybe JSS
  }

classes :: ListItemClassKey
classes =
  { root : Nothing
  , container : Nothing
  , focusVisible : Nothing
  , dense : Nothing
  , alignItemsFlexStart : Nothing
  , disabled : Nothing
  , divider : Nothing
  , gutters : Nothing
  , button : Nothing
  , secondaryAction : Nothing
  , selected : Nothing
  }

listItem :: { | ListItemProps } -> JSX
listItem props = do
  let foreignProps = write (toInternalChildren props) 
  element _ListItem (unsafeCoerce foreignProps)


foreign import _ListItem :: ∀ a. ReactComponent a