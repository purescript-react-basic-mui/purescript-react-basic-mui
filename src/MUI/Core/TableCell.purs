module MUI.Core.TableCell where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_td)
import Unsafe.Coerce (unsafeCoerce)

type TableCellProps componentProps =
  ( align :: String
  , children :: Array JSX
  , classes :: TableCellClassKey
  , component :: ReactComponent { | componentProps }
  , padding :: String
  , scope :: String
  , size :: String
  , sortDirection :: String
  , variant :: String
  | componentProps
  )

foreign import data TableCellClassKey :: Type
foreign import data TableCellPropsPartial :: Type

type TableCellClassKeyOptions =
  ( root :: String
  , head :: String
  , body :: String
  , footer :: String
  , sizeSmall :: String
  , paddingCheckbox :: String
  , paddingNone :: String
  , alignLeft :: String
  , alignCenter :: String
  , alignRight :: String
  , alignJustify :: String
  )

tableCellClassKey :: ∀ options options_
  . Union options options_ TableCellClassKeyOptions
  => Record options
  -> TableCellClassKey
tableCellClassKey = unsafeCoerce

tableCellPropsPartial_component :: ∀ componentProps props props_
  .  Union props props_ (TableCellProps componentProps)
  => Record props 
  -> TableCellPropsPartial
tableCellPropsPartial_component = unsafeCoerce

tableCellPropsPartial :: ∀ props props_
  .  Union props props_ (TableCellProps Props_td)
  => Record props 
  -> TableCellPropsPartial
tableCellPropsPartial = unsafeCoerce

tableCell_component :: ∀ componentProps props props_
  . Union props props_ (TableCellProps componentProps)
  => Record props 
  -> JSX
tableCell_component = element _TableCell

tableCell :: ∀ props props_
  . Union props props_ (TableCellProps Props_td)
  => Record props 
  -> JSX
tableCell = element _TableCell

foreign import  _TableCell :: ∀ a. ReactComponent a