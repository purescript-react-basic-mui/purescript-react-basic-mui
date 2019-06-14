module React.Basic.MUI.ListItem where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, ReactComponent, JSX)
import React.Basic.MUI.ButtonBase (ExtendButtonBase)

type ListItemTypeMap_required optional p d =
  ( props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  | optional )

type ListItemTypeMap_optional p d =
  ( 
  )

foreign import data ListItemTypeMap :: Type Type

listItemTypeMap
  :: ∀ attrs attrs_
   . Union attrs attrs_ (ListItemTypeMap_optional)
  => Record (ListItemTypeMap_required attrs)
  -> ListItemTypeMap
listItemTypeMap = unsafeCoerce

listItem :: Foreign
listItem = _ListItem
foreign import _ListItem :: Foreign

type ListItemClassKey = Foreign

type ListItemProps d p = OverrideProps ListItemTypeMap Foreign Foreign Foreign