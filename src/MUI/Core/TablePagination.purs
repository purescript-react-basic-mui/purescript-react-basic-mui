module MUI.Core.TablePagination where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Foreign (Foreign, unsafeToForeign)
import Foreign.Object (Object)
import MUI.Core.IconButton (IconButtonProps)
import MUI.Core.Internal (toInternalChildren)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.Events (EventHandler)
import Record as Record
import Simple.JSON (write)
import Unsafe.Coerce (unsafeCoerce)

type TablePaginationActions = Object Foreign
type SelectProps = Object Foreign

type TablePaginationProps =
  ( "ActionsComponent" :: Maybe TablePaginationActions
  , backIconButtonProps :: Maybe { | IconButtonProps }
  , classes :: TablePaginationClassKey
  , component :: Maybe String
  , count :: Maybe Number
  , labelDisplayedRows :: Maybe ({ from :: Number, to :: Number, count :: Number } -> String)
  , labelRowsPerPage :: Maybe String
  , nextIconButtonProps :: Maybe { | IconButtonProps }
  , onChangePage :: Maybe EventHandler
  , onChangeRowsPerPage :: Maybe EventHandler
  , page :: Maybe Number
  , rowsPerPage :: Maybe Number
  , rowsPerPageOptions :: Maybe (Array Number)
  , "SelectProps" :: Maybe SelectProps
  , align :: Maybe String
  , padding :: Maybe String
  , scope :: Maybe String
  , size :: Maybe String
  , sortDirection :: Maybe String
  , variant :: Maybe String
  )


tablePaginationProps :: { | TablePaginationProps }
tablePaginationProps = 
  { "ActionsComponent" : Nothing
  , backIconButtonProps : Nothing
  , classes
  , component : Nothing
  , count : Nothing
  , labelDisplayedRows : Nothing
  , labelRowsPerPage : Just "Rows per page:"
  , nextIconButtonProps : Nothing
  , onChangePage : Nothing
  , onChangeRowsPerPage : Nothing
  , page : Nothing
  , rowsPerPage : Nothing
  , rowsPerPageOptions : Just [ 10.0, 25.0, 50.0, 100.0]
  , "SelectProps" : Nothing
  , align : Just "inherit"
  , padding : Nothing
  , scope : Nothing
  , size : Nothing
  , sortDirection : Nothing
  , variant : Nothing
  }

type TablePaginationClassKey =
  { root :: Maybe String
  , toolbar :: Maybe String
  , spacer :: Maybe String
  , caption :: Maybe String
  , selectRoot :: Maybe String
  , select :: Maybe String
  , selectIcon :: Maybe String
  , input :: Maybe String
  , menuItem :: Maybe String
  , actions :: Maybe String
  }

classes :: TablePaginationClassKey
classes =
  { root : Nothing
  , toolbar : Nothing
  , spacer : Nothing
  , caption : Nothing
  , selectRoot : Nothing
  , select : Nothing
  , selectIcon : Nothing
  , input : Nothing
  , menuItem : Nothing
  , actions : Nothing
  }

tablePagination :: { | TablePaginationProps } -> JSX
tablePagination props = do
  let foreignBackIconButtonProps = write <$> toInternalChildren <$> props.backIconButtonProps
  let foreignNackIconButtonProps = write <$> toInternalChildren <$> props.nextIconButtonProps
  let foreignLabelDisplayRows = unsafeToForeign <$> props.labelDisplayedRows
      newProps = Record.set (SProxy :: SProxy "backIconButtonProps") foreignBackIconButtonProps
                   $ Record.set (SProxy :: SProxy "nextIconButtonProps") foreignNackIconButtonProps
                   $ Record.set (SProxy :: SProxy "onChangePage") (unsafeToForeign <$> props.onChangePage)
                   $ Record.set (SProxy :: SProxy "labelDisplayedRows") (unsafeToForeign <$> props.labelDisplayedRows)
                   $ Record.set (SProxy :: SProxy "onChangeRowsPerPage") (unsafeToForeign <$> props.onChangeRowsPerPage) props
  element _TablePagination (unsafeCoerce $ write newProps)


foreign import  _TablePagination :: ∀ a. ReactComponent a
