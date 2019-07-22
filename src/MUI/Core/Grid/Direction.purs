module MUI.Core.Grid.Direction where

import Unsafe.Coerce (unsafeCoerce)

foreign import data DirectionProp :: Type

data Direction = Row | RowReverse | Column | ColumnReverse

direction :: Direction -> DirectionProp
direction Row = unsafeCoerce "row"
direction RowReverse = unsafeCoerce "row-reverse"
direction Column = unsafeCoerce "column"
direction ColumnReverse = unsafeCoerce "column-reverse"