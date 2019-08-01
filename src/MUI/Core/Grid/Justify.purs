module MUI.Core.Grid.Justify where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data JustifyProp :: Type
foreign import _eqJustifyProp :: JustifyProp -> JustifyProp -> Boolean
foreign import _ordJustifyProp :: JustifyProp -> JustifyProp -> Int
instance eqJustifyProp :: Eq JustifyProp where eq left right = _eqJustifyProp left right
instance ordJustifyProp :: Ord JustifyProp where compare left right = compare (_ordJustifyProp left right) (_ordJustifyProp right left)

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