module React.Basic.MUI.Core.ListItem where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Core.ButtonBase (ExtendButtonBase)
import React.Basic.MUI.Core.OverridableComponent (OverrideProps)

type ListItemTypeMap_required  p d optional =
  ( props :: Foreign
  , defaultComponent :: Foreign
  , classKey :: Foreign
  | optional )

type ListItemTypeMap_optional p d =
  ( 
  )

foreign import data ListItemTypeMap :: Type  -> Type



listItem :: Foreign
listItem = _ListItem
foreign import _ListItem :: Foreign

type ListItemClassKey = Foreign

type ListItemProps d p = OverrideProps ListItemTypeMap Foreign Foreign Foreign