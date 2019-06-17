module React.Basic.MUI.Core.Breadcrumbs where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

breadcrumbs :: Foreign
breadcrumbs = _Breadcrumbs
foreign import _Breadcrumbs :: Foreign

type BreadcrumbsClassKey = Foreign

type BreadcrumbsProps = SimplifiedPropsOf Breadcrumbs