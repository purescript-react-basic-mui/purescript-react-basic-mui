module React.Basic.MUI.Core.MenuItem where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.ListItem (ListItemTypeMap)
import React.Basic.MUI.Core.ButtonBase (ExtendButtonBase)
import React.Basic.MUI.Core.OverridableComponent (OverrideProps)

type MenuItemClassKey = Foreign

menuItem :: Foreign
menuItem = _MenuItem
foreign import _MenuItem :: Foreign

type MenuItemProps d p = OverrideProps ListItemTypeMap Foreign Foreign Foreign