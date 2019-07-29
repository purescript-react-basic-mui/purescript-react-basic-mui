module MUI.Core.ListItem where

import Foreign (Foreign)
import Foreign.Object (Object)
import MUI.Core (JSS)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_li, Props_div)
import Unsafe.Coerce (unsafeCoerce)

type ContainerProps = Object Foreign

type ListItemProps componentProps containerProps = 
  ( alignItems :: AlignItemsProp
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

foreign import data AlignItemsProp :: Type
data AlignItems = FlexStart | Start | Center
alignItems :: AlignItems -> AlignItemsProp
alignItems FlexStart = unsafeCoerce "flex-start"
alignItems Start = unsafeCoerce "start"
alignItems Center = unsafeCoerce "center"

foreign import data ListItemClassKey :: Type
foreign import data ListItemClassKeyJSS :: Type
foreign import data ListItemPropsPartial :: Type

type ListItemClassKeyOptionsJSS = ListItemClassKeyOptionsR JSS
type ListItemClassKeyOptions = ListItemClassKeyOptionsR String
type ListItemClassKeyOptionsR a = 
  ( root :: a
  , container :: a
  , focusVisible :: a
  , dense :: a
  , alignItemsFlexStart :: a
  , disabled :: a
  , divider :: a
  , gutters :: a
  , button :: a
  , secondaryAction :: a
  , selected :: a
  )

listItemClassKey :: ∀ options options_
  . Union options options_ ListItemClassKeyOptions
  => Record options
  -> ListItemClassKey
listItemClassKey = unsafeCoerce

listItemClassKeyJSS :: ∀ options options_
  . Union options options_ ListItemClassKeyOptionsJSS
  => Record options
  -> ListItemClassKeyJSS
listItemClassKeyJSS = unsafeCoerce

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