module MUI.Core.Grid.Justify where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data JustifyProp :: Type
foreign import _eqJustifyProp :: JustifyProp -> JustifyProp -> Boolean
foreign import _ordJustifyProp :: JustifyProp -> JustifyProp -> Int
instance eqJustifyProp :: Eq JustifyProp where eq _left _right = _eqJustifyProp _left _right
instance ordJustifyProp :: Ord JustifyProp where compare _left _right = compare (_ordJustifyProp _left _right) (_ordJustifyProp _right _left)

flexStart :: JustifyProp
flexStart = unsafeCoerce "flex-start"

center :: JustifyProp
center = unsafeCoerce "center"

flexEnd :: JustifyProp
flexEnd = unsafeCoerce "flex-end"

spaceBetween :: JustifyProp
spaceBetween = unsafeCoerce "space-between"

spaceAround :: JustifyProp
spaceAround = unsafeCoerce "space-around"

spaceEvenly :: JustifyProp
spaceEvenly = unsafeCoerce "space-evenly"