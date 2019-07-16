module MUI.Core.ListItem where

import Foreign (Foreign)
import Foreign.Object (Object)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_li, Props_div)
import Unsafe.Coerce (unsafeCoerce)

type ContainerProps = Object Foreign

type ListItemProps componentProps containerProps = 
  ( alignItems :: String
  , autoFocus :: Boolean
  , button :: Boolean
  , children :: Array JSX
  , classes :: ListItemClassKey
  , component :: ReactComponent { | componentProps }
  , "ContainerComponent" :: { | containerProps }
  , "ContainerProps" :: ReactComponent { | containerProps }
  , dense :: Boolean
  , disabled :: Boolean
  , disableGutters :: Boolean
  , divider :: Boolean
  , selected  :: Boolean
  | componentProps
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

listItemPropsPartial_component :: ∀ componentProps containerProps props props_
  . Union props props_ (ListItemProps  componentProps containerProps)
  => Record props 
  -> ListItemPropsPartial 
listItemPropsPartial_component = unsafeCoerce

listItemPropsPartial :: ∀ props props_
  . Union props props_ (ListItemProps Props_li Props_div)
  => Record props 
  -> ListItemPropsPartial 
listItemPropsPartial = unsafeCoerce

listItem_component :: ∀ componentProps containerProps props props_
  . Union props props_ (ListItemProps componentProps containerProps)
  => Record props 
  -> JSX
listItem_component = element _ListItem

listItem :: ∀ props props_
  . Union props props_ (ListItemProps Props_li Props_div)
  => Record props 
  -> JSX
listItem = element _ListItem


foreign import _ListItem :: ∀ a. ReactComponent a