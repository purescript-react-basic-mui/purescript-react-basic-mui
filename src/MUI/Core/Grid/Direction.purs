module MUI.Core.Grid.Direction where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data DirectionProp :: Type
foreign import _eqDirectionProp :: DirectionProp -> DirectionProp -> Boolean
foreign import _ordDirectionProp :: DirectionProp -> DirectionProp -> Int
instance eqDirectionProp :: Eq DirectionProp where eq _left _right = _eqDirectionProp _left _right
instance ordDirectionProp :: Ord DirectionProp where compare _left _right = compare (_ordDirectionProp _left _right) (_ordDirectionProp _right _left)

row :: DirectionProp
row = unsafeCoerce "row"

rowReverse :: DirectionProp
rowReverse = unsafeCoerce "row-reverse"

column :: DirectionProp
column = unsafeCoerce "column"

columnReverse :: DirectionProp
columnReverse = unsafeCoerce "column-reverse"