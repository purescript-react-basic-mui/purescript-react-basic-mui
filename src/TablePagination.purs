module React.Basic.MUI.TablePagination where 

import Prelude
import Foreign (Foreign)
import Foreign.Object (Object)
import React.Basic (Component, JSX)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Events (EventHandler)


type LabelDisplayedRowsArgs  =
  { from :: Number
  , to :: Number
  , count :: Number
  , page :: Number
  }

type LabelDisplayedRowsArgs_required =
  ( from :: Number
  , to :: Number
  , count :: Number
  , page :: Number
  )

type LabelDisplayedRowsArgs_optional =
  ( 
  )

tablePagination :: Foreign
tablePagination = _TablePagination
foreign import _TablePagination :: Foreign