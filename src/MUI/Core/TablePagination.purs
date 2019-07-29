module MUI.Core.TablePagination where

import Prelude

import Effect.Uncurried (EffectFn2)
import Foreign (Foreign)
import Foreign.Object (Object)
import MUI.Core (JSS)
import MUI.Core.IconButton (IconButtonPropsPartial)
import MUI.Core.Table as Table
import MUI.Core.TableCell (AlignProp, SortDirectionProp, VariantProp)
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_div)
import React.Basic.Events (EventHandler, SyntheticEvent)
import Unsafe.Coerce (unsafeCoerce)

type TablePaginationActions = Object Foreign
type SelectProps = Object Foreign

type TablePaginationProps componentProps =
  ( "ActionsComponent" :: TablePaginationActions
  , align :: AlignProp
  , backIconButtonProps :: IconButtonPropsPartial
  , classes :: TablePaginationClassKey
  , component :: ReactComponent { | componentProps } 
  , count :: Number
  , labelDisplayedRows :: ({ from :: Number, to :: Number, count :: Number } -> String)
  , labelRowsPerPage :: String
  , nextIconButtonProps :: IconButtonPropsPartial
  , onChangePage :: EffectFn2 SyntheticEvent Number Unit
  , onChangeRowsPerPage :: EventHandler
  , page :: Number
  , rowsPerPage :: Number
  , rowsPerPageOptions :: (Array Number)
  , "SelectProps" :: SelectProps
  , padding :: Table.PaddingProp
  , scope :: String
  , size :: Table.SizeProp
  , sortDirection :: SortDirectionProp
  , variant :: VariantProp
  | componentProps
  )

foreign import data TablePaginationClassKey :: Type
foreign import data TablePaginationClassKeyJSS :: Type
foreign import data TablePaginationPropsPartial :: Type

type TablePaginationClassKeyOptionsJSS = TablePaginationClassKeyOptionsR JSS
type TablePaginationClassKeyOptions = TablePaginationClassKeyOptionsR String
type TablePaginationClassKeyOptionsR a =
  ( root :: a
  , toolbar :: a
  , spacer :: a
  , caption :: a
  , selectRoot :: a
  , select :: a
  , selectIcon :: a
  , input :: a
  , menuItem :: a
  , actions :: a
  )

tablePaginationClassKey :: ∀ options options_
  . Union options options_ TablePaginationClassKeyOptions
  => Record options
  -> TablePaginationClassKey
tablePaginationClassKey = unsafeCoerce

tablePaginationClassKeyJSS :: ∀ options options_
  . Union options options_ TablePaginationClassKeyOptionsJSS
  => Record options
  -> TablePaginationClassKeyJSS
tablePaginationClassKeyJSS = unsafeCoerce

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
