module MUI.Core.ListItem where

import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type ContainerProps = Object Foreign

type ListItemProps = 
  ( alignItems :: String
  , autoFocus :: Boolean
  , button :: Boolean
  , children :: Array JSX
  , classes :: ListItemClassKey
  , "ContainerComponent" :: String
  , "ContainerProps" :: ContainerProps
  , dense :: Boolean
  , disabled :: Boolean
  , disableGutters :: Boolean
  , divider :: Boolean
  , selected  :: Boolean
  )

foreign import data ListItemClassKey :: Type
foreign import data ListItemPropsPartial :: Type

type ListItemClassKeyOptions =
  ( root :: String
  , container :: String
  , focusVisible :: String
  , dense :: String
  , alignItemsFlexStart :: String
  , disabled :: String
  , divider :: String
  , gutters :: String
  , button :: String
  , secondaryAction :: String
  , selected :: String
  )

listItemClassKey :: ∀ options options_
  . Union options options_ ListItemClassKeyOptions
  => Record options
  -> ListItemClassKey
listItemClassKey = unsafeCoerce

listItemPropsPartial :: ∀ props props_
  . Union props props_ ListItemProps
  => Record props 
  -> ListItemPropsPartial 
listItemPropsPartial = unsafeCoerce


listItem :: ∀ props props_
  . Union props props_ ListItemProps
  => Record props 
  -> JSX
listItem = element _ListItem


foreign import _ListItem :: ∀ a. ReactComponent a