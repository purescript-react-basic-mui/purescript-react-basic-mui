module MUI.Core.Styles.MakeStyles where

import MUI.Core.Styles.Types (Theme)
import React.Basic.Hooks (Hook)
import Type.RowList (class ListToRow, class RowToList, Cons, Nil, kind RowList)

foreign import data UseStyles :: Type -> Type

makeStyles :: forall input output. MapRecordValuesToString input output => (Theme -> Record input) -> Hook UseStyles (Record output)
makeStyles f = makeStylesImpl f

-- | Unsafe FFI call without constraints.
-- | Constrainted FFI functions won't be supported by purs soon.
foreign import makeStylesImpl :: ∀ input output. (Theme -> Record input) -> Hook UseStyles (Record output)

class MapRecordValuesToString (input :: # Type) (output :: # Type)

instance mapRecordValuesToString ::
  ( RowToList i il
  , MapRecordValuesToStringImpl il ol
  , ListToRow ol o
  ) =>
  MapRecordValuesToString i o

class MapRecordValuesToStringImpl (il :: RowList) (ol :: RowList) | il -> ol

instance mapRecordValuesToStringImplNil ::
  MapRecordValuesToStringImpl Nil Nil

instance mapRecordValuesToStringImplCons ::
  (MapRecordValuesToStringImpl it ot) =>
  MapRecordValuesToStringImpl
    (Cons s a it)
    (Cons s String ot)
