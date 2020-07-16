module MUI.Core.Styles.MakeStyles where

import Type.RowList (class ListToRow, class RowToList, Cons, Nil, kind RowList)
import MUI.Core.Styles.CreateMuiTheme (Theme)
import React.Basic.Hooks (Hook)

foreign import data UseStyles :: Type -> Type

foreign import makeStyles :: forall input output. MapRecordValuesToString input output => (Theme -> Record input) -> Hook UseStyles (Record output)

class MapRecordValuesToString (input_row :: #Type) (output_row :: #Type)

instance mapRecordValuesToString ::
  ( RowToList input_row input_rowList
  , MapRecordValuesToStringImpl input_rowList output_rowList
  , ListToRow output_rowList output_row
  ) =>
  MapRecordValuesToString input_row output_row

class MapRecordValuesToStringImpl (input_rowList :: RowList) (output_rowList :: RowList) | input_rowList -> output_rowList

instance mapRecordValuesToStringImplNil ::
  MapRecordValuesToStringImpl Nil Nil

instance mapRecordValuesToStringImplCons ::
  (MapRecordValuesToStringImpl input_tail output_tail) =>
  MapRecordValuesToStringImpl
    (Cons input_symbol input_fieldType input_tail)
    (Cons input_symbol String output_tail)
