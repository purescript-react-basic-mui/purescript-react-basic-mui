module React.Basic.MUI.Core.TablePagination where 

import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX)
import React.Basic.MUI.Core.TablePagination.TablePaginationActions (TablePaginationActionsProps)
import React.Basic.MUI.Core.IconButton (IconButtonProps)
import React.Basic.Events (EventHandler)
import React.Basic.MUI.Core.Select (SelectProps)
import React.Basic.MUI.Core.TableCell (TableCellProps)
import React.Basic.MUI.Core.OverridableComponent (SimplifiedPropsOf)

type LabelDisplayedRowsArgs_required  optional =
  ( from :: Number
  , to :: Number
  , count :: Number
  , page :: Number
  | optional )

type LabelDisplayedRowsArgs_optional =
  ( 
  )

foreign import data LabelDisplayedRowsArgs :: Type 



tablePagination :: Foreign
tablePagination = _TablePagination
foreign import _TablePagination :: Foreign

type TablePaginationClassKey = Foreign

type TablePaginationBaseProps = Foreign

type TablePaginationProps = SimplifiedPropsOf TablePagination