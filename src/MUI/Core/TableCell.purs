module MUI.Core.TableCell where

import MUI.Core.Table as Table
import Prim.Row (class Union)
import React.Basic (JSX, ReactComponent, element)
import React.Basic.DOM (Props_td)
import Unsafe.Coerce (unsafeCoerce)

type TableCellProps componentProps =
  ( align :: AlignProp
  , children :: Array JSX
  , classes :: TableCellClassKey
  , component :: ReactComponent { | componentProps }
  , padding :: Table.PaddingProp
  , scope :: String
  , size :: Table.SizeProp
  , sortDirection :: SortDirectionProp
  , variant :: VariantProp
  | componentProps
  )

foreign import data AlignProp :: Type
data Align = Inherit | Left | Center | Right | Justify
align :: Align -> AlignProp
align Inherit = unsafeCoerce "inherit"
align Left = unsafeCoerce "left"
align Center = unsafeCoerce "center"
align Right = unsafeCoerce "right"
align Justify = unsafeCoerce "justify"

foreign import data SortDirectionProp :: Type
data SortDirection = Asc | Desc | False
sortDirection :: SortDirection -> SortDirectionProp
sortDirection Asc = unsafeCoerce "asc"
sortDirection Desc = unsafeCoerce "desc"
sortDirection False = unsafeCoerce false

foreign import data VariantProp :: Type
data Variant = Head | Body | Footer
variant :: Variant -> VariantProp
variant Head = unsafeCoerce "head"
variant Body = unsafeCoerce "body"
variant Footer = unsafeCoerce "footer"

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