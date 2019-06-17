module React.Basic.MUI.Core.TableSortLabel where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.Core.ButtonBase (ExtendButtonBase)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Core.SvgIcon (SvgIconProps)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

tableSortLabel :: Foreign
tableSortLabel = _TableSortLabel
foreign import _TableSortLabel :: Foreign

type TableSortLabelClassKey = Foreign

type TableSortLabelProps = SimplifiedPropsOf TableSortLabel