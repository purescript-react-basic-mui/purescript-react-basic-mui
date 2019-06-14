module React.Basic.MUI.Breadcrumbs where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX, ReactComponent)

breadcrumbs :: Foreign
breadcrumbs = _Breadcrumbs
foreign import _Breadcrumbs :: Foreign

type BreadcrumbsClassKey = Foreign

type BreadcrumbsProps = SimplifiedPropsOf Breadcrumbs