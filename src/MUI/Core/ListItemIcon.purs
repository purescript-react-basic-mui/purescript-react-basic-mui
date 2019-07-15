module MUI.Core.ListItemIcon where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type ListItemIconProps =
  ( children :: Array JSX
  , classes :: ListItemIconClassKey
  )

foreign import data ListItemIconClassKey :: Type
foreign import data ListItemIconPropsPartial :: Type

type ListItemIconClassKeyOptions =
  ( root :: String
  , alignItemsFlexStart :: String
  )

listItemIconClassKey :: ∀ options options_
  . Union options options_ ListItemIconClassKeyOptions
  => Record options
  -> ListItemIconClassKey
listItemIconClassKey = unsafeCoerce

listItemIconPropsPartial :: ∀ props props_
  . Union props props_ ListItemIconProps
  => Record props 
  -> ListItemIconPropsPartial 
listItemIconPropsPartial = unsafeCoerce

listItemIcon :: ∀ props props_
  . Union props props_ ListItemIconProps
  => Record props 
  -> JSX
listItemIcon = element _ListItemIcon


foreign import _ListItemIcon :: ∀ a. ReactComponent a