module MUI.Core.ListItemIcon where

import Data.Maybe (Maybe)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type ListItemIconProps =
  ( children :: Maybe (Array JSX)
  , classes :: ListItemIconClassKey
  )

foreign import data ListItemIconClassKey :: Type

type ListItemIconClassKeyOptions =
  ( root :: String
  , alignItemsFlexStart :: String
  )

listItemIconClassKey 
  :: ∀ options options_
  . Union options options_ ListItemIconClassKeyOptions
  => Record options
  -> ListItemIconClassKey
listItemIconClassKey = unsafeCoerce

listItemIcon
  :: ∀ props props_
  . Union props props_ ListItemIconProps
  => Record props 
  -> JSX
listItemIcon = element _ListItemIcon


foreign import _ListItemIcon :: ∀ a. ReactComponent a