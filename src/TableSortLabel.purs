module React.Basic.MUI.TableSortLabel where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic.MUI.ButtonBase (ExtendButtonBase)
import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.SvgIcon (SvgIconProps)

tableSortLabel :: ReactComponent
tableSortLabel = _TableSortLabel
foreign import _TableSortLabel :: ReactComponent

type TableSortLabelClassKey = Foreign

type TableSortLabelProps = SimplifiedPropsOf TableSortLabel