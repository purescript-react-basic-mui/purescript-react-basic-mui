module React.Basic.MUI.TablePagination where 

import Prelude
import Prim.Row (class Union)
import Unsafe.Coerce (unsafeCoerce)
import Foreign (Foreign)


import React.Basic (element, ReactComponent, JSX, ReactComponent)
import React.Basic.MUI.TablePagination.TablePaginationActions (TablePaginationActionsProps)
import React.Basic.MUI.IconButton (IconButtonProps)
import React.Basic.Events (EventHandler)
import React.Basic.MUI.Select (SelectProps)

type LabelDisplayedRowsArgs_required optional =
  ( from :: Number
  , to :: Number
  , count :: Number
  , page :: Number
  | optional )

type LabelDisplayedRowsArgs_optional =
  ( 
  )

foreign import data LabelDisplayedRowsArgs :: Type 

labelDisplayedRowsArgs
  :: ∀ attrs attrs_
   . Union attrs attrs_ (LabelDisplayedRowsArgs_optional)
  => Record (LabelDisplayedRowsArgs_required attrs)
  -> LabelDisplayedRowsArgs
labelDisplayedRowsArgs = unsafeCoerce

tablePagination :: Foreign
tablePagination = _TablePagination
foreign import _TablePagination :: Foreign

type TablePaginationClassKey = Foreign

type TablePaginationBaseProps = Omit TableCellProps  Foreign

type TablePaginationProps = SimplifiedPropsOf TablePagination