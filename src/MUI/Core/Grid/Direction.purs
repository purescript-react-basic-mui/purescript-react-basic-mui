module MUI.Core.Grid.Direction where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data DirectionProp :: Type
foreign import _eqDirectionProp :: DirectionProp -> DirectionProp -> Boolean
foreign import _ordDirectionProp :: DirectionProp -> DirectionProp -> Int
instance eqDirectionProp :: Eq DirectionProp where eq left right = _eqDirectionProp left right
instance ordDirectionProp :: Ord DirectionProp where compare left right = compare (_ordDirectionProp left right) (_ordDirectionProp right left)

row :: DirectionProp
row = unsafeCoerce "row"

rowReverse :: DirectionProp
rowReverse = unsafeCoerce "row-reverse"

column :: DirectionProp
column = unsafeCoerce "column"

columnReverse :: DirectionProp
columnReverse = unsafeCoerce "column-reverse"