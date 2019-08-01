module MUI.Core.Grid.Wrap where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data WrapProp :: Type
foreign import _eqWrapProp :: WrapProp -> WrapProp -> Boolean
foreign import _ordWrapProp :: WrapProp -> WrapProp -> Int
instance eqWrapProp :: Eq WrapProp where eq _left _right = _eqWrapProp _left _right
instance ordWrapProp :: Ord WrapProp where compare _left _right = compare (_ordWrapProp _left _right) (_ordWrapProp _right _left)

nowrap :: WrapProp
nowrap = unsafeCoerce "nowrap"

wrap :: WrapProp
wrap = unsafeCoerce "wrap"

wrapReverse :: WrapProp
wrapReverse = unsafeCoerce "wrap-reverse"