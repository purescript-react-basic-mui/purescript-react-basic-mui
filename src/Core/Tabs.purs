module React.Basic.MUI.Core.Tabs where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Core.Tabs.TabIndicator (TabIndicatorProps)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

tabs :: Foreign
tabs = _Tabs
foreign import _Tabs :: Foreign

type TabsClassKey = Foreign

type TabsActions_required  optional =
  ( updateIndicator :: Foreign
  | optional )

type TabsActions_optional =
  ( 
  )

foreign import data TabsActions :: Type 



type TabsProps = SimplifiedPropsOf Tabs