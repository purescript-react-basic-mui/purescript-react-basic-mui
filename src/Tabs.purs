module React.Basic.MUI.Tabs where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX, ReactComponent)
import React.Basic.MUI.Tabs.TabIndicator (TabIndicatorProps)

tabs :: Foreign
tabs = _Tabs
foreign import _Tabs :: Foreign

type TabsClassKey = Foreign

type TabsActions_required optional =
  ( updateIndicator :: Foreign
  | optional )

type TabsActions_optional =
  ( 
  )

foreign import data TabsActions :: Type 

tabsActions
  :: ∀ attrs attrs_
   . Union attrs attrs_ (TabsActions_optional)
  => Record (TabsActions_required attrs)
  -> TabsActions
tabsActions = unsafeCoerce

type TabsProps = SimplifiedPropsOf Tabs