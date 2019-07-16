module MUI.Core.TablePagination where

import Foreign (Foreign)
import Foreign.Object (Object)
import MUI.Core.IconButton (IconButtonPropsPartial)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import React.Basic.Events (EventHandler)
import Unsafe.Coerce (unsafeCoerce)

type TablePaginationActions = Object Foreign
type SelectProps = Object Foreign

type TablePaginationProps componentProps =
  ( "ActionsComponent" :: TablePaginationActions
  , backIconButtonProps :: IconButtonPropsPartial
  , classes :: TablePaginationClassKey
  , component :: ReactComponent { | componentProps } 
  , count :: Number
  , labelDisplayedRows :: ({ from :: Number, to :: Number, count :: Number } -> String)
  , labelRowsPerPage :: String
  , nextIconButtonProps :: IconButtonPropsPartial
  , onChangePage :: EventHandler
  , onChangeRowsPerPage :: EventHandler
  , page :: Number
  , rowsPerPage :: Number
  , rowsPerPageOptions :: (Array Number)
  , "SelectProps" :: SelectProps
  , align :: String
  , padding :: String
  , scope :: String
  , size :: String
  , sortDirection :: String
  , variant :: String
  | componentProps
  )

foreign import data TablePaginationClassKey :: Type
foreign import data TablePaginationPropsPartial :: Type

type TablePaginationClassKeyOptions =
  ( root :: String
  , toolbar :: String
  , spacer :: String
  , caption :: String
  , selectRoot :: String
  , select :: String
  , selectIcon :: String
  , input :: String
  , menuItem :: String
  , actions :: String
  )

tablePaginationClassKey :: ∀ options options_
  . Union options options_ TablePaginationClassKeyOptions
  => Record options
  -> TablePaginationClassKey
tablePaginationClassKey = unsafeCoerce

tablePaginationPropsPartial :: ∀ props props_
  . Union props props_ (TablePaginationProps Props_div)
  => Record props 
  -> TablePaginationPropsPartial
tablePaginationPropsPartial = unsafeCoerce

tablePaginationPropsPartial_component :: ∀ componentProps props props_
  . Union props props_ (TablePaginationProps componentProps )
  => Record props 
  -> TablePaginationPropsPartial
tablePaginationPropsPartial_component = unsafeCoerce


tablePagination_component :: ∀ componentProps props props_
  . Union props props_ (TablePaginationProps componentProps )
  => Record props 
  -> JSX
tablePagination_component = element _TablePagination

tablePagination :: ∀ props props_
  . Union props props_ (TablePaginationProps Props_div)
  => Record props 
  -> JSX
tablePagination = element _TablePagination

foreign import  _TablePagination :: ∀ a. ReactComponent a
