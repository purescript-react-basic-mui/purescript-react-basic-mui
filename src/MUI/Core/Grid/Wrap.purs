module MUI.Core.Grid.Wrap where

import Prelude

import Unsafe.Coerce (unsafeCoerce)

foreign import data WrapProp :: Type
foreign import _eqWrapProp :: WrapProp -> WrapProp -> Boolean
foreign import _ordWrapProp :: WrapProp -> WrapProp -> Int
instance eqWrapProp :: Eq WrapProp where eq left right = _eqWrapProp left right
instance ordWrapProp :: Ord WrapProp where compare left right = compare (_ordWrapProp left right) (_ordWrapProp right left)

nowrap :: WrapProp
nowrap = unsafeCoerce "nowrap"

wrap :: WrapProp
wrap = unsafeCoerce "wrap"

wrapReverse :: WrapProp
wrapReverse = unsafeCoerce "wrap-reverse"