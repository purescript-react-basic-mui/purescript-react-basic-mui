module React.Basic.MUI.MenuItem where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, ReactComponent)
import React.Basic.MUI.ListItem (ListItemTypeMap)
import React.Basic.MUI.ButtonBase (ExtendButtonBase)

type MenuItemClassKey = Foreign

menuItem :: Foreign
menuItem = _MenuItem
foreign import _MenuItem :: Foreign

type MenuItemProps d p = OverrideProps ListItemTypeMap Foreign Foreign Foreign