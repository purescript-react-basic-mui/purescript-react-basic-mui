module MUI.Core.TableCell where

import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import Unsafe.Coerce (unsafeCoerce)

type TableCellProps =
  ( align :: String
  , children :: Array JSX
  , classes :: TableCellClassKey
  , component :: String
  , padding :: String
  , scope :: String
  , size :: String
  , sortDirection :: String
  , variant :: String
  )

foreign import data TableCellClassKey :: Type

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

tableCellClassKey 
  :: ∀ options options_
  . Union options options_ TableCellClassKeyOptions
  => Record options
  -> TableCellClassKey
tableCellClassKey = unsafeCoerce

tableCell
  :: ∀ props props_
  . Union props props_ TableCellProps
  => Record props 
  -> JSX
tableCell = element _TableCell

foreign import  _TableCell :: ∀ a. ReactComponent a